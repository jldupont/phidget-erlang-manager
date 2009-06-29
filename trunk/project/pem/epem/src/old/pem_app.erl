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
%% MESSAGES GENERATED:
%% ===================
%%
%% daemonized
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

-define(TIMEOUT, 3000).

-define(SUBS, [control, daemon_pid, daemon_exit]).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 stop/0,
	 start/0,
	 start/1,
	 loop/0,
	 loop_main/0
        ]).

-export([
		 start_daemon/1
		 ]).

-export([
		 hevent/1
		 ]).

%% ====================================================================!
%% API functions
%% ====================================================================!

%% START
start() ->
	start_daemon([{debug, false}]).
	
%%	base:ilog(?MODULE, "start~n", []),
%%	start_daemon(undefined).

%% START
start([debug]) ->
	base:ilog(?MODULE, "start(debug)~n", []),
	start_daemon([{debug, true}]).

start_daemon(Args) ->
	process_flag(trap_exit,true),
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	Args2=lists:append(Args, [{root, Pid}]),
	base:ilog(?MODULE, "start_daemon: Pid[~p] Args[~p]~n", [Args2]),
	put(args, Args2),	
	put(context, client),
	Pid ! {start_daemon, Args2},
	{ok, Pid}.

	

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
			pem_sup:start_link(Args),
			
			reflector:sync_to_reflector(?SUBS),
			put(state, try_start),
			daemon_ctl:start_link(),
			daemon_ctl:start_daemon(Args);

		{ready, From, Msg} ->
			hevent({ready, From, Msg});

		
		Other->
			base:elog(?MODULE, "received unknown message [~p]~n", [Other])

	after ?TIMEOUT ->
		 
		reflector:sync_to_reflector(?SUBS)
		
	end,    %%loop
	loop().


hevent({ready, From, Msg}) ->
	ok.



%% Main loop - does not have
%% a timer constraint -> less CPU overhead
%%
%% We need to respond to messages/commands sent
%% from management Client.
%%
loop_main() ->
	receive
		
		stop ->
			base:ilog(?MODULE, "exiting~n", []),
			exit(ok);
		
		{daemon_exit, Pid} ->
			base:ilog(?MODULE, "exiting daemon, Pid[~p]~n", [Pid]);
	
		Other -> 
			base:ilog(?MODULE, "received unexpected Message[~p]~n", [Other])
	
	after ?TIMEOUT ->
			
		reflector:sync_to_reflector(?SUBS)			
	
	end,
	loop_main().
