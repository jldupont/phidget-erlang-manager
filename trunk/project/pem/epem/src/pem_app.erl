%% Author: Jean-Lou Dupont
%% Created: 2009-06-22
%% Description: TODO: Add description to pem
%%
%% start()            -> starts daemon
%% start(debug)       -> starts daemon
%% start(debug,start) -> starts daemon
%%
%% start(debug,stop)  -> stops daemon
%% start(stop)        -> stops daemon
%%
-module(pem_app).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 stop/0,
	 start/0,
	 start/1,
	 start/2,
	 loop/0,
	 try_start/1,
	 try_start/2,
	 really_start/1
        ]).

-export([
		 start_daemon/1,
		 stop_daemon/1
		 ]).

%% ====================================================================!
%% API functions
%% ====================================================================!

%% START
start() ->
	start_daemon([]).

%% START
start(debug) ->
	start_daemon([debug]);

%% ##STOP##
start(stop) ->
	stop_daemon([]).

%% START
start(debug, start) ->
	start_daemon([debug]);

%% ##STOP##
start(debug, stop) ->
	stop_daemon([debug]).


%% 1- Is the daemon running already?
%%    - Retrieving port from CTL file
%%      - Start Client side comm
%%    - Try establishing comm, retrieve PID
%%    - Not responding? clear CTL file
%%
%% 2- Not running:
%%    - Start daemon
%%    - Write comm port to CTL file
%%
start_daemon(Args) ->
	base:ilog(?MODULE, "start_daemon: Args[~p]~n", [Args]),
	put(args, Args),
	process_flag(trap_exit,true),
	?MODULE ! {start_daemon, Args},
	loop().

stop_daemon(Args) ->
	base:ilog(?MODULE, "stop_daemon: Args[~p]~n", [Args]),
	put(args, Args),
	?MODULE ! {stop_daemon, Args},
	loop().


	

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
    ?MODULE ! stop.


%% ====================================================================!
%% LOCAL functions
%% ====================================================================!

loop() ->
	receive
		
		%% Try starting the daemon
		{start_daemon, Args} ->
			put(state, try_start),
			try_start(Args);

		{stop_daemon, Args} ->
			put(state, try_stop);

		
		%% Socket to daemon server
		%% opened... act upon 
		{from_server, {info, open}} ->
			ok;
		
		stop ->
			exit(ok);
		
		Other->
			base:elog(?MODULE, "received unknown message [~p]~n", [Other])

	end,
	loop().


try_start(Args) ->
	Port=daemon_ctl:getport(),
	try_start(Args, Port).
	
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
