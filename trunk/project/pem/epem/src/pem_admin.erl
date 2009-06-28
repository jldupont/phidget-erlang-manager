%% Author:      Jean-Lou Dupont
%% Created:     2009-06-27
%% Description: TODO: Add description to pem_admin
%%
%% USES:
%% ====
%% - reflector
%% - base
%% - daemon_client
%%
-module(pem_admin).

%%
%% Macros
%%
-define(TIMEOUT, 2000).


-define(CANSTART,       0).
-define(STOPSENT,       0).
-define(EUCMD,          1).
-define(CANNOTSTOP,     2).
-define(COMMERROR,      3).
-define(DAEMON_PRESENT, 4).
-define(NODAEMON,       5).
-define(EUNKNOWN,       10).


-define(SUBS, [
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
	
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	?MODULE ! {run, Cmd},
	{ok, Pid}.
	



gevent(E) ->
	?MODULE ! E.



hevent(E) ->
	Cmd   = get(cmd),
	State = get(state),
	hcevent(Cmd, State, E).




loop() ->
	receive
		
		{run, Cmd} ->
			put(cmd, Cmd),
			put(state, run),
			pem_admin_sup:start_link({?MODULE, ready});
			%%reflector:sync_to_reflector(?SUBS)

		synced ->
			hevent(synced);
		
		{sync, From, ready} ->
			base:ilog(?MODULE, "module [~p] is ready~n", [From]),
			put({sync, From}, true),
			hevent({sync, From});
		
		stop ->
			exit(ok);
		

		{port, Port} ->
			hevent({port, Port});
	
		%%from daemon_client
		{management, Msg} ->
			hevent({management, Msg});
		
		%%from daemon_client on behalf of daemon
		{from_daemon, Msg} ->
			hevent({from_daemon, Msg});
		
		Other ->
			io:format("something is wrong... unhandled event[~p]~n", [Other])
	
	
	after ?TIMEOUT ->
			
		reflector:sync_to_reflector(?SUBS)
	
	end,	
	loop().


hcevent(_, _, {sync, _From}) ->
	Count = base:pvadd(module_synced, 1),
	case Count of
		1 ->
			ok;
		2 ->
			put(state, synced),
			gevent(synced)
	end;

	
%% Try to start a daemon
%%      Cmd, State, Event
hcevent(_  , _    , synced) ->
	Port=base:getport(),
	put(state, synced),
	gevent( {port, Port} );	

%% We've got a valid port... let's try to connect
hcevent(_, synced, {port, {port, Port}} ) ->
	put(state, try_connect),
	reflector:send(pem_admin, management_port, Port),
	reflector:send(pem_admin, client, connect),
	ok;

%% Can't get a port... no daemon (probably)
hcevent(start, run, {port, _} ) ->
	io:format("no management port found~n"),
	halt(?CANSTART);
	
%% We've got a port opened ... possibly to the daemon
hcevent(start, tryconnect, {management, open}) ->
	put(state, wait_pid),
	reflector:send(pem_admin, to_daemon, {asked_pid, what_pid}),
	ok;

hcevent(start, wait_pid, {management, {txerror, _}}) ->
	io:format("communication error to daemon~n"),
	halt(?COMMERROR);

%% Message was sent ok... wait for a reply
hcevent(start, wait_pid, {management, {txok, _}}) ->
	ok;

%% We received a pid from the daemon... this means
%% one is (probably) running -> we can't start another one!
hcevent(start, wait_pid, {from_daemon, {pid, Pid}}) ->
	io:format("daemon already running, Pid[~p]~n", [Pid]),
	halt(?DAEMON_PRESENT);	

hcevent(start, tryconnect, {management, _Other}) ->
	io:format("communication error to daemon~n"),
	halt(?COMMERROR);


%% =================== STOP ========================


%% Can't get a port... no daemon (probably)
hcevent(stop, synced, {port, _} ) ->
	io:format("no management port found~n"),
	halt(?CANNOTSTOP);


%% We've got a port opened ... possibly to the daemon
hcevent(stop, tryconnect, {management, open}) ->
	put(state, wait_pid),
	reflector:send(pem_admin, to_daemon, {asked_exit, do_exit}),
	ok;

hcevent(stop, tryconnect, {management, {txok, _}}) ->
	io:format("stop command sent~n"),
	halt(?STOPSENT);

hcevent(stop, tryconnect, {management, _Other}) ->
	io:format("no daemon found~n"),
	halt(?NODAEMON);

hcevent(Cmd, State, Event) ->
	io:format(">>> something is wrong... Cmd[~p] State[~p] Event[~p]~n", [Cmd, State, Event]),
	halt(?EUNKNOWN).




