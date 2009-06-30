%% Author:      Jean-Lou Dupont
%% Created:     2009-06-27
%% Description: TODO: Add description to pem_admin
%%
%% USES:
%% ====
%% - switch
%% - base
%% - daemon_client
%%
-module(pem_admin).

%%
%% Macros
%%
-define(TIMEOUT, 2000).


-define(CANSTART,       0).
-define(STOPSENT,       1).
-define(EUCMD,          2).
-define(CANNOTSTOP,     3).
-define(COMMERROR,      4).
-define(DAEMON_PRESENT, 5).
-define(NODAEMON,       6).
-define(EUNKNOWN,       10).


-define(SUBS, [ 
			   ready,
			   management,
			   from_daemon
			   ]).

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start/1,
		 stop/0,
		 loop/0,
		 run/1
		 ]).

-export([
		 gevent/1,
		 hevent/1,
		 hcevent/3
		 ]).

%% Switch duties
-export([
		 loop_switch/0
		 ]).

%% =========================================================================
%% API Functions
%% =========================================================================

start() ->
	io:format("pem_admin [start|stop]~n").

start([start]) ->
	run(start);

start([stop]) ->
	run(stop);

start([Unknown]) ->
	io:format("pem_admin: unknown command[~p]~n", [Unknown]),
	halt(?EUCMD);

start(Unknown) ->
	io:format("pem_admin: unknown command[~p]~n", [Unknown]),
	halt(?EUCMD).


stop() ->
	?MODULE ! stop.



%% =========================================================================
%% Local Functions
%% =========================================================================

run(Cmd) ->
	
	%% Register ourselves as message switch
	SPid = spawn(?MODULE, loop_switch, []),
	register(switch, SPid),

	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	
	?MODULE ! {run, Cmd},
	{ok, Pid}.
	


%% ====================================================================!
%% LOCAL functions
%% ====================================================================!
loop_switch() ->
	receive
		
		%% SWITCH DUTY: subscribe
		{From, subscribe, Type} ->
			%%base:ilog(?MODULE, "switch: subscribe: From[~p] Type[~p]~n", [From, Type]),
			switch:add_subscriber(From, Type);
		
		%% SWITCH DUTY: publish
		{From, publish, {MsgType, Msg}} ->
			%%base:ilog(?MODULE, "switch: publish: From[~p] Type[~p] Msg[~p]~n", [From, MsgType, Msg]),
			switch:do_publish(From, MsgType, Msg);
	
		Other ->
			base:elog(?MODULE, "switch: ERROR, invalid [~p]~n", [Other])
			
	end,
	loop_switch().



gevent(E) ->
	?MODULE ! E.



hevent(E) ->
	Cmd   = get(cmd),
	State = get(state),
	hcevent(Cmd, State, E).




loop() ->
	receive
		
		%% We don't really need the feedback...
		%% Just suppress it.
		{switch, subscribed} ->
			pem_admin_sup:start_link();			

		
		%% Step #1
		{run, Cmd} ->
			switch:subscribe(?MODULE, ?SUBS),			
			put(cmd, Cmd),
			put(state, run);


		%% Accumulate modules ready
		%% In Step #1, a module sends the following message
		%% once it is ready to process more messages ie.
		%% its message loop is ready.
		{daemon_client, ready, _} ->
			put(state, ready),
			hevent(ready);
			
		
		stop ->
			exit(ok);
		

		{port, Port} ->
			hevent({port, Port});
	
		%%from daemon_client
		{_From, management, Msg} ->
			%%io:format("management: ~p~n", [Msg]),
			hevent({management, Msg});
		
		%%from daemon_client on behalf of daemon
		{_From, from_daemon, Msg} ->
			%%io:format("from_daemon: ~p~n", [Msg]),
			hevent({from_daemon, Msg});
		
		Other ->
			io:format("something is wrong... unhandled event[~p]~n", [Other])
	
	
	after ?TIMEOUT ->
			
		hevent(timeout)
	
	end,	
	loop().



	
%% Try to start a daemon
%%      Cmd, State, Event

hcevent(_, _, ready) ->
	
	Porte=base:getport(),
	case Porte of
		{port, Port} when is_number(Port) ->
			put(state, tryconnect),
			switch:publish(?MODULE, client, {doconnect, Port});
		_ ->
			halt(?CANSTART)
	end;


hcevent(_, tryconnect, timeout) ->
	halt(?COMMERROR);


%% We've got a port opened ... possibly to the daemon
hcevent(start, tryconnect, {management, open}) ->
	put(state, wait_pid),
	switch:publish(?MODULE, to_daemon, {asked_pid, what_pid}),	
	ok;

hcevent(_, wait_pid, timeout) ->
	halt(?COMMERROR);

hcevent(start, wait_pid, {management, {txerror, _}}) ->
	%%io:format("communication error to daemon~n"),
	halt(?COMMERROR);

%% Message was sent ok... wait for a reply
hcevent(start, wait_pid, {management, {txok, _}}) ->
	ok;

%% We received a pid from the daemon... this means
%% one is (probably) running -> we can't start another one!
hcevent(start, wait_pid, {from_daemon, {pid, _Pid}}) ->
	%%io:format("daemon already running, Pid[~p]~n", [Pid]),
	halt(?DAEMON_PRESENT);	

hcevent(start, tryconnect, {management, _Other}) ->
	%%io:format("communication error to daemon~n"),
	halt(?COMMERROR);


%% =================== STOP ========================



%% We've got a port opened ... possibly to the daemon
hcevent(stop, tryconnect, {management, open}) ->
	put(state, wait_pid),
	switch:publish(?MODULE, to_daemon, {asked_exit, do_exit}),	
	ok;

hcevent(stop, tryconnect, {management, {txok, _}}) ->
	%%io:format("stop command sent~n"),
	halt(?STOPSENT);

hcevent(stop, tryconnect, {management, _Other}) ->
	%%io:format("no daemon found~n"),
	halt(?NODAEMON);

hcevent(stop, wait_pid, {management, {txok, _}}) ->
	%%io:format("stop command sent~n"),
	halt(?STOPSENT);

hcevent(stop, wait_pid, {management, {txerror, _}}) ->
	%%io:format("stop command sent~n"),
	halt(?COMMERROR);


hcevent(_, _, timeout) ->
	ok;

hcevent(Cmd, State, Event) ->
	io:format(">>> something is wrong... Cmd[~p] State[~p] Event[~p]~n", [Cmd, State, Event]),
	halt(?EUNKNOWN).




