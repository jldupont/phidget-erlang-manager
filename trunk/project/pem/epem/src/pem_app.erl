%% Author: Jean-Lou Dupont
%% Created: 2009-06-22
%% Description: TODO: Add description to pem
%%
%% API:
%%
%%   start()            -> starts daemon
%%   start(debug)       -> starts daemon
%%   start(debug,start) -> starts daemon
%%
%%   start(debug,stop)  -> stops daemon
%%   start(stop)        -> stops daemon
%%
%%
%% SUBSCRIPTIONS:
%% ==============
%%
%% {control, canstart}
%% {control, daemon_not_found}
%% {daemon_pid, Pid}
%%
%%
%% Contexts:
%% 
%%  - Client (for managing a running daemon)
%%  - Daemon (a running daemon)
%%
%% States:
%% 
%%  - Client
%%    * try_start:  trying to start a daemon
%%    * try_stop:   trying to stop a running daemon
%%    
%%
-module(pem_app).

-define(TIMEOUT, 2000).

-define(SUBS, [daemon_pid]).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 stop/0,
	 start/0,
	 start/1,
	 start/2,
	 loop/0,
	 loop_main/0,
	 try_start/1,
	 try_start/2,
	 try_stop/1,
	 try_stop/2,
	 handle_command/0
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
	put(context, client),
	base:ilog(?MODULE, "start_daemon: Args[~p]~n", [Args]),
	put(args, Args),
	process_flag(trap_exit,true),
	?MODULE ! {start_daemon, Args},
	loop().

stop_daemon(Args) ->
	put(context, client),
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
			{control,canstart} ->
			ok;
		

		
		
		%% Try starting the daemon
		{start_daemon, Args} ->
			put(state, try_start),
			try_start(Args);

		%% Try stopping the daemon
		{stop_daemon, Args} ->
			put(state, try_stop),
			try_stop(Args);

		%% Tried to stop a running
		%% daemon but didn't find one
		daemon_not_found ->
			io:format("cannot find daemon~n"),
			exit(ok);

		%% Socket to daemon server
		%% opened... act upon 
		{from_server, {info, open}} ->
			put(state, daemon_found),
			handle_command();

		%% The daemon server returns its Pid
		{from_server, {message, {pid, Pid}}} ->
			put(daemon_pid, Pid),
			self() ! got_pid;

		%% we got the pid from the running daemon...
		%% now, what were we trying todo?
		%% 1- start: then, abort!
		%% ... just this one case for now.
		got_pid ->
			Pid = get(daemon_pid),
			io:format("daemon already running, pid[~p]~n", [Pid]),
			exit(ok);

		stop ->
			io:format("exiting~n"),
			base:ilog(?MODULE, "exiting~n"),
			exit(ok);
		
		Other->
			base:elog(?MODULE, "received unknown message [~p]~n", [Other])

	after ?TIMEOUT ->
		  
		State = get(state),
		case State of
			
			started ->
				loop_main();
			
			%% We asked the supposedly running daemon
			%% for its pid but it took too long... assume it isn't
			%% really a daemon we know about.
			asked_pid ->
				put(state, start),
				self() ! start,
				loop_main();
			
			
			%% We asked the daemon (which we, at this point, believe
			%% is running) to exit... make sure it has.
			%% TODO to better than this
			asked_exit ->
				io:format("daemon should have terminated~n"),
				ok;

			
			%% Couldn't communicate with a daemon
			%% So probably none were present... let's start
			%% then!
			try_start ->
				put(state, start),
				self() ! start,
				loop_main();
			
			%% Couldn't communicate with a daemon
			%% no use trying to stop then... exit
			try_stop ->
				io:format("cannot communicate with the daemon~n"),
				exit(ok)


		end %%case
	
	end,    %%loop
	loop().



%% Main loop - does not have
%% a timer constraint -> less CPU overhead
%%
%% We need to respond to messages/commands sent
%% from management Client.
%%
loop_main() ->
	receive
		
		start ->
			put(context, daemon),
			Args=get(args),
			pem_sup:start_link(Args);

	
		%% A management Client is making sure
		%% we are alive... 
		{from_client, {message, what_pid}} ->
			base:ilog(?MODULE, "replying with Pid[~p] in response to management request~n", [self()]),
			%% TODO send message back
			ok;
	
		{from_client, {message, do_exit}} ->
			base:ilog(?MODULE, "exiting in response to management request~n"),
			exit(ok)
	
	
	end,
	loop_main().


%% The supported commands are:
%% 1) start
%% 2) stop
handle_command() ->
	State = get(state),
	case State of
		
		%% we've got a communication channel down to 
		%% the daemon... ask for its Pid to make sure
		%% we really have a PEM daemon at hand
		try_start ->
			put(state, asked_pid),
			daemon_client:send_message(asked_pid, what_pid);

		%% ok then, ask the daemon to exit
		try_stop ->
			put(state, asked_exit),
			daemon_client:send_message(asked_exit, do_exit)

	end.


