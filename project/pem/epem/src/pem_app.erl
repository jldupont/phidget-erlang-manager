%% Author: Jean-Lou Dupont
%% Created: 2009-06-22
%% Description: TODO: Add description to pem
%%
%% Duties:
%% =======
%%
%% - Start Supervisor tree
%% - Start Daemon
%% - Act a Message Switch
%%   - support subscribe / unsubscribe
%% - Stop Daemon on command from pem_admin
%%
%%
%%
-module(pem_app).

-define(TIMEOUT, 3000).

-define(SUBS, [ready, control, daemon_pid, daemon_exit]).

%% --------------------------------------------------------------------
%% COMMAND LINE API
%% --------------------------------------------------------------------
-export([
		 start/0,
		 start/1
		 ]).

%% --------------------------------------------------------------------
%% DEBUG API
%% --------------------------------------------------------------------
-export([
	 stop/0
        ]).

%% --------------------------------------------------------------------
%% INTERNAL API
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%% --------------------------------------------------------------------
-export([
	 loop/0
        ]).


-export([
		 start_daemon/1
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
	start_daemon([{debug, true}]).

start_daemon(Args) ->
	base:ilog(?MODULE, "start_daemon~n",[]),
	process_flag(trap_exit,true),
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	
	%% Register ourselves as message switch
	register(switch, Pid),
	
	Args2=lists:append(Args, [{root, Pid}]),
	base:ilog(?MODULE, "start_daemon: Pid[~p] Args[~p]~n", [Args2]),
	put(args, Args2),
	
	?MODULE ! {args, Args2},
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
		{args, Args} ->
			put(args, Args),
			pem_sup:start_link(Args);
		
		stop ->
			base:ilog(?MODULE, "exiting~n", []),
			halt();

		
		{_From, daemon_exit, _} ->
			?MODULE ! stop;
		

		{From, ready, Pid} ->
			base:ilog(?MODULE, "ready [~p][~p]~n", [From, Pid]);
			
		
		%% SWITCH DUTY: subscribe
		{From, subscribe, Type} ->
			switch:add_subscriber(From, Type);
		
		%% SWITCH DUTY: publish
		{From, publish, {MsgType, Msg}} ->
			switch:publish(From, MsgType, Msg);
		
		Other->
			base:elog(?MODULE, "received unknown message [~p]~n", [Other])

	end,    %%loop
	loop().


