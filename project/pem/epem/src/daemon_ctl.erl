%% Author: Jean-Lou Dupont
%% Created: 2009-06-23
%% Description: Control for the daemon
%%
%%
%% SUBSCRIPTIONS:
%% ==============
%%  
%%  {assignedport, Port}
%%
%%
%% MESSAGE GENERATED:
%% ==================
%%
%%

-module(daemon_ctl).

%% Timeout for communicating
%% with the daemon
-define(TIMEOUT, 2000).

%% Reflector subscriptions
-define(SUBS, [assignedport]).


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
		 try_stop/2
		 ]).

%% =======================================
%% API Functions
%% =======================================
start() ->
	Pid = spawn(?MODULE, loop, []),
	register(daemon_ctl, Pid),
	{ok, Pid}.
	
start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register(daemon_ctl, Pid),
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
		{assignedport, Port} ->
			saveport(Port);
		
		%% TODO is there a daemon already running?
		%%      Wait x time for response back

		{command, start, Args} ->
			put(context, start),
			try_start(Args);
	
		{command, stop, Args} ->
			put(context, stop),
			try_stop(Args)
		
	end,
	loop().




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
	daemon_client:start_link(Port, self(), from_server);
	
						
%% Didn't find a port... but
%% there could still be a zombie/unreachable
%% daemon lying around... can't do anything from here
try_start(Args, _) ->
	put(state, started),
	pem_sup:start_link(Args).


%% Trying to stop an active daemon
try_stop(Args) ->
	Port=daemon_ctl:getport(),
	try_stop(Args, Port).

try_stop(_Args, {port, Port}) ->
	daemon_client:start_link(Port, self(), from_server);

%% Can't find a communication channel down to daemon...
%% Maybe the daemon just isn't there of course.
try_stop(_Args, _) ->
	self() ! daemon_not_found.


