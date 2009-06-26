%% Author: Jean-Lou Dupont
%% Created: 2009-06-23
%% Description: Control for the daemon
%%
%%
%% SUBSCRIPTIONS:
%% ==============
%%  
%%  {assignedport, Port}
%%  {management,  X}
%%  {from_daemon, M}
%%  {from_client, M}
%%
%%
%% MESSAGE GENERATED:
%% ==================
%%
%% {control,     canstart}
%% {control,     stop_sent_ok}
%% {control,     stop_sent_error}
%% {control,     daemon_not_found}
%% {daemon_pid,  Pid}
%% {daemon_exit, Pid}
%%
%% MESSAGES SENT TO THE CLIENT:
%% {pid, Pid}
%%

-module(daemon_ctl).

%% Timeout for communicating
%% with the daemon
-define(TIMEOUT, 2000).

%% Reflector subscriptions
-define(SUBS, [assignedport, management, from_daemon, daemonized, from_client]).


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
		 hcevent/4
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
			put(command, none),
			put(state,  started),
			put(context, client);
		
		{assignedport, Port} ->
			saveport(Port);
		
		%% TODO is there a daemon already running?
		%%      Wait x time for response back

		{command, start, Args} ->
			put(args, Args),
			put(state,   started),
			put(command, start),
			put(context, client),
			hevent(command_start);
	
		{command, stop, Args} ->
			put(args,    Args),
			put(state,   started),
			put(command, stop),
			put(context, client),
			hevent(command_stop);			

		cannot_reach_daemon ->
			hevent(cannot_reach_daemon);
		
		{daemonized} ->
			hevent(daemonized);
		
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


hevent(E) ->
	Command = get(command),
	Context = get(context),
	State   = get(state),
	base:ilog(?MODULE, "hevent: Context[~p] State[~p] Command[~p]~n",[Context, State, Command]),	
	hcevent(Context, State, Command, E).
	



%%               CLIENT CONTEXT
%%
%%      Context, State, Command, Msg
%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


hcevent(client, started, start, command_start) ->
	Args=get(args),
	try_start(Args),
	put(state, wait_management);

hcevent(client, started, start, command_stop) ->
	Args=get(args),
	try_stop(Args),
	put(state, wait_management);



%% Management channel found: ask for PID to running daemon
hcevent(client, wait_management, start, {management, open}) ->
	reflector:send_sync(self(), to_daemon, {asked_pid, what_pid}, ?SUBS),  %% {to_daemon, {asked_pid, what_pid}}
	put(state, wait_pid);

%% Couldn't reach a running daemon
hcevent(client, wait_management, start, timeout) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, canstart, ?SUBS);           %% {control, canstart}

%% We received a PID; that means that a daemon is effectively running
%% and we can't be starting a new one!
hcevent(client, wait_pid, start, {from_daemon, {pid, Pid}}) ->
	put(state, daemon_present),
	reflector:send_sync(self(), daemon_pid, Pid, ?SUBS);             %% {daemon_pid, Pid}
	
%% Timed-out whilst waiting for a PID from the daemon...
%% assume the "best case" i.e. no daemon running
hcevent(client, wait_pid, start, timeout) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, canstart, ?SUBS);           %% {control, canstart}

%% Out "what_pid" was successfully sent... good
hcevent(client, wait_pid, start, {management, {txok, _MsgId} }) ->
	ok;

%% Error in transmitting to daemon... assume "best case"
hcevent(client, wait_pid, start, {management, {_, _MsgId} }) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, canstart, ?SUBS);           %% {control, canstart}



%%%%CATCH-ALL%%%%
hcevent(client, State, start, Event) ->
	base:elog(?MODULE, "hcevent: INVALID PATTERN, Client: State[~p] Command[Start] Event[~p]~n", [State, Event]);


%% STOP COMMAND
%% ^^^^^^^^^^^^


%% Management channel found: ask the running daemon to stop
hcevent(client, wait_management, stop, {management, open}) ->
	reflector:send_sync(self(), to_daemon, {asked_exit, do_exit}, ?SUBS),   %% {to_daemon, {asked_exit, do_exit}}
	put(state, wait_txok_exit);

%% We sent a 'stop' command to the daemon and our request
%% was transmitted OK.
hcevent(client, wait_txok_exit, stop, {management, {txok, _X} }) ->
	reflector:send_sync(self(), control, stop_sent_ok, ?SUBS),              %% {control, stop_sent_ok}	
	put(state, stop_sent_ok);


%% We sent a 'stop' command *BUT* the transmission wasn't successful...
hcevent(client, wait_txok_exit, stop, {management, {txerror, _X} }) ->
	reflector:send_sync(self(), control, stop_sent_error, ?SUBS),           %% {control, stop_sent_error}
	put(state, stop_sent_error);

%% We sent a 'stop' command *BUT* the management channel went down 
hcevent(client, wait_txok_exit, stop, {management, _X }) ->
	reflector:send_sync(self(), control, stop_sent_error, ?SUBS),           %% {control, stop_sent_error}
	put(state, stop_sent_error);

%% Couldn't reach a running daemon
hcevent(client, wait_management, stop, timeout) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, daemon_not_found, ?SUBS);          %% {control, daemon_not_found}


%%%%CATCH-ALL%%%%
hcevent(client, State, stop, Event) ->
	base:elog(?MODULE, "hcevent: INVALID PATTERN, Client: State[~p] Command[Stop] Event[~p]~n", [State, Event]);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%               DAEMON SIDE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The management client is asking us to exit daemon state...
hcevent(daemon, started, _, {from_client, do_exit}) ->
	Pid = os:getpid(),
	reflector:send_sync(self(), daemon_exit, Pid, ?SUBS);                   %% {daemon_exit, Pid}


%% The management client is inquiring about our PID...
hcevent(daemon, started, _, {from_client, what_pid}) ->
	Pid = os:getpid(),
	reflector:send_sync(self(), to_client, {pid, Pid}, ?SUBS);              %% {to_client, {pid, Pid}}

%%%%CATCH-ALL%%%%
hcevent(daemon, State, Command, Event) ->
	base:elog(?MODULE, "hcevent: INVALID PATTERN, Daemon: State[~p] Command[~p] Event[~p]~n", [State, Command, Event]);


hcevent(Context, State, Command, Event) ->
	base:elog(?MODULE, "hcevent: INVALID PATTERN, Context[~p] State[~p] Command[~p] Event[~p]~n", [Context, State, Command, Event]).






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
	?MODULE ! cannot_reach_daemon;


%% Found a port... but is the daemon
%% really there and active?
try_start(_Args, {port, Port}) ->
	daemon_client:start(Port);
	
						
%% Didn't find a port... but
%% there could still be a zombie/unreachable
%% daemon lying around... can't do anything from here
try_start(_Args, _) ->
	?MODULE ! cannot_reach_daemon.


%% Trying to stop an active daemon
try_stop(Args) ->
	Port=daemon_ctl:getport(),
	try_stop(Args, Port).

try_stop(_Args, {port, Port}) ->
	daemon_client:start(Port);

%% Can't find a communication channel down to daemon...
%% Maybe the daemon just isn't there of course.
try_stop(_Args, _) ->
	?MODULE ! cannot_reach_daemon,
	reflector:send_sync(self(), control, daemon_not_found, ?SUBS).



