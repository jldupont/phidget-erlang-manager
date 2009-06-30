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
		 start_daemon/1,
		 loop_switch/0
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
	
	%% Register ourselves as message switch
	SPid = spawn(?MODULE, loop_switch, []),
	register(switch, SPid),
	
	Pid = spawn(?MODULE, loop, []),
	register(?MODULE, Pid),
	
	Args2=lists:append(Args, [{root, Pid}]),
	base:ilog(?MODULE, "start_daemon: Pid[~p] Args[~p]~n", [Pid, Args2]),
	put(args, Args2),
	
	?MODULE ! {args, Args2},
	loop().
	%%{ok, Pid}.


%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
    ?MODULE ! stop.


%% ====================================================================!
%% LOCAL functions
%% ====================================================================!
loop_switch() ->
	receive
		
		%% SWITCH DUTY: subscribe
		{From, subscribe, Type} ->
			base:ilog(?MODULE, "switch: subscribe: From[~p] Type[~p]~n", [From, Type]),
			switch:add_subscriber(From, Type);
		
		%% SWITCH DUTY: publish
		{From, publish, {MsgType, Msg}} ->
			%%base:ilog(?MODULE, "switch: publish: From[~p] Type[~p] Msg[~p]~n", [From, MsgType, Msg]),
			switch:do_publish(From, MsgType, Msg);
	
		Other ->
			base:elog(?MODULE, "switch: ERROR, invalid [~p]~n", [Other])
			
	end,
	loop_switch().



loop() ->
	receive
		{args, Args} ->
			put(args, Args),
			switch:subscribe(pem_app, ?SUBS),
			pem_sup:start_link(Args);
		
		stop ->
			base:ilog(?MODULE, "exiting~n", []),
			halt();

		{switch, subscribed} ->
			ok;
		
		{_From, daemon_exit, _} ->
			?MODULE ! stop;
		

		{_From, ready, _Pid} ->
			ok;
			%%base:ilog(?MODULE, "ready [~p][~p]~n", [From, Pid]);
			
		
		
		Other->
			base:elog(?MODULE, "received unknown message [~p]~n", [Other])

	%%after 5000 ->
			
		%%base:ilog(?MODULE, "timeout!~n",[])
	
	end,    %%loop
	loop().


