%% Author: Jean-Lou Dupont
%% Created: 2009-06-23
%% Description: Control for the daemon
%%
%%
%% SUBSCRIPTIONS:
%% ==============
%%  
%%  {assignedport, Port}
%%  {management, X}
%%  {from_daemon, Y}
%%
%% MESSAGE GENERATED:
%% ==================
%%
%% {control, stop_sent_ok}
%% {control, stop_sent_error}
%%
%%

-module(daemon_ctl).

%% Timeout for communicating
%% with the daemon
-define(TIMEOUT, 2000).

%% Reflector subscriptions
-define(SUBS, [assignedport, management, from_daemon]).


%% =============================
%% API Exported Functions
%% =============================
-export([
		 start/0,
		 start_link/0,
		 start_daemon/1,
		 stop_daemon/1,
		 getpid_daemon/0,
		 getpid_daemon/1
		 ]).

%%
%% LOCAL Exported Functions
%%
-export([
		 loop/0,
		 getport/0,
		 saveport/1,
		 extract_port/1,
		 try_start/1,
		 try_start/2,
		 try_stop/1,
		 try_stop/2,
		 hevent/1,
		 hcevent/3
		 ]).

%% =======================================
%% API Functions
%% =======================================
start() ->
	Pid = spawn(?MODULE, loop, []),
	register(daemon_ctl, Pid),
	Pid ! started,
	{ok, Pid}.
	
start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register(daemon_ctl, Pid),
	Pid ! started,
	{ok, Pid}.


start_daemon(Args) ->
	?MODULE ! {command, start, Args},
	ok.
	
stop_daemon(Args) ->
	?MODULE ! {command, stop, Args}.

	

%% ====================================================================================
%% Local Functions
%% ====================================================================================

%% Returns the Port# of the
%% daemon currently running (if any)
%%
%% Returns: {port, Port}
%%
getport() ->
	{Code, X} = base:read_ctl_file(),
	Terms=X,
	case Code of 
		ok ->
			extract_port(Terms);
		_ ->
			% error code really
			{Code, X}
	end.

extract_port(Terms) ->
	case erlang:is_builtin(lists, keyfind, 3) of
		true  ->
			lists:keyfind(port,1,Terms);
		false ->
			case lists:keysearch(port,1,Terms) of
				{value, Value} ->
					Value;
				_ ->
					error
			end
	end.


%% Save the used by this daemon
saveport(Port) ->
	base:ilog(?MODULE, "saved daemon port[~p]~n",[Port]),
	base:create_ctl_file([{port, Port}]).



%% ================================================================
%% MAIN LOOP
%% ================================================================
loop() ->
	receive
		
		started ->
			reflector:subscribe(daemon_ctl, ?SUBS),
			put(context, started);
		
		{assignedport, Port} ->
			saveport(Port);
		
		%% TODO is there a daemon already running?
		%%      Wait x time for response back

		{command, start, Args} ->
			put(args, Args),
			put(context, start),
			try_start(Args);
	
		{command, stop, Args} ->
			put(args, Args),
			put(context, stop),
			try_stop(Args);
	
		%% @TODO
		{from_daemon, Msg} ->
			hevent({from_daemon, Msg});
		
		{management, Msg} ->
			hevent({management, Msg});
		
		Other ->
			base:ilog(?MODULE, "received [~p]~n", [Other])
	
	after ?TIMEOUT ->

		reflector:sync_to_reflector(?SUBS),
		hevent(timeout)
	
	end,
	loop().

hevent(timeout) ->
	Context = get(context),
	State   = get(state),
	hcevent(Context, State, timeout);	

hevent({from_daemon, M}) ->
	Context = get(context),
	State   = get(state),
	hcevent(Context, State, {from_daemon, M});

hevent({management, M}) ->
	Context=get(context),
	State   = get(state),
	hcevent(Context, State, {management, M}).


%% START CONTEXT
%% ^^^^^^^^^^^^^

%% Management channel found: ask for PID to running daemon
hcevent(start, wait_management, {management, open}) ->
	reflector:send_sync(self(), to_daemon, {asked_pid, what_pid}, ?SUBS),
	put(state, wait_pid);

%% Couldn't reach a running daemon
hcevent(start, wait_management, timeout) ->
	put(state, canstart),
	reflector:send_sync(self(), control, canstart, ?SUBS);


%% Management
hcevent(start, wait_management, {management, closed}}) ->
	put(state, canstart),
	reflector:send_sync(self(), control, canstart, ?SUBS);



%% STOP CONTEXT
%% ^^^^^^^^^^^^

%% Management channel found: ask the running daemon to stop
hcevent(stop, wait_management, {management, open}) ->
	reflector:send_sync(self(), to_daemon, {asked_exit, do_exit}, ?SUBS),
	put(state, wait_txok_exit);

%% We sent a 'stop' command to the daemon and our request
%% was transmitted OK.
hcevent(stop, wait_txok_exit, {management, {txok, _X} }) ->
	reflector:send_sync(self(), control, stop_sent_ok, ?SUBS),	
	put(state, stop_sent_ok);


%% We sent a 'stop' command *BUT* the transmission wasn't successful...
hcevent(stop, wait_txok_exit, {management, {txerror, _X} }) ->
	reflector:send_sync(self(), control, stop_sent_error, ?SUBS),
	put(state, stop_sent_error);

%% We sent a 'stop' command *BUT* the management channel went down 
hcevent(stop, wait_txok_exit, {management, _X }) ->
	reflector:send_sync(self(), control, stop_sent_error, ?SUBS),
	put(state, stop_sent_error);

%% Couldn't reach a running daemon
hcevent(stop, wait_management, timeout) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, daemon_not_found, ?SUBS).


  



%% Asks the daemon side for its Pid.
%% The answer will be relayed to the process configured
%% through the "set_routeto" function.
getpid_daemon() ->
	Port=?MODULE:getport(),
	getpid_daemon(Port).

getpid_daemon({port, Port}) ->
	daemon_client:send_command(Port, {command, pid});

getpid_daemon({Code, Error}) ->
	{Code, Error}.




try_start(Args) ->
	Port=daemon_ctl:getport(),
	try_start(Args, Port).
	
%% Can't find port in CTL file...
try_start(_Args, {error, _X}) ->
	?MODULE ! port_not_found;


%% Found a port... but is the daemon
%% really there and active?
try_start(_Args, {port, Port}) ->
	daemon_client:start(Port),
	put(state, wait_for_management);
	
						
%% Didn't find a port... but
%% there could still be a zombie/unreachable
%% daemon lying around... can't do anything from here
try_start(_Args, _) ->
	put(state, canstart),
	reflector:send_sync(self(), control, canstart, ?SUBS).


%% Trying to stop an active daemon
try_stop(Args) ->
	Port=daemon_ctl:getport(),
	try_stop(Args, Port).

try_stop(_Args, {port, Port}) ->
	daemon_client:start(Port),
	put(state, wait_for_management);

%% Can't find a communication channel down to daemon...
%% Maybe the daemon just isn't there of course.
try_stop(_Args, _) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, daemon_not_found, ?SUBS).



