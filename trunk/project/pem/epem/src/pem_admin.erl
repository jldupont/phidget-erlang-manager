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
	io:format("pem_admin: unknown command[~p]~n", [Unknown]);

start(Unknown) ->
	io:format("pem_admin: unknown command[~p]~n", [Unknown]).


stop() ->
	?MODULE ! stop.



%% =========================================================================
%% Local Functions
%% =========================================================================

run(Cmd) ->
	
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	?MODULE ! run,
	?MODULE ! {cmd, Cmd},
	{ok, Pid}.
	





gevent(E) ->
	?MODULE ! E.



hevent(E) ->
	Cmd   = get(cmd),
	State = get(state),
	hcevent(Cmd, State, E).




loop() ->
	receive
		
		run ->
			put(cmd, undefined),
			put(state, run),
			pem_admin_sup:start_link();	
		
		stop ->
			exit(ok);
		
		{cmd, Cmd} ->
			put(cmd, Cmd),
			hevent({cmd, Cmd}),
			io:format("Command[~p]~n", [Cmd]);

		{port, Port} ->
			hevent({port, Port});
	
		Other ->
			io:format("something is wrong... unhandled event[~p]~n", [Other])
	
	
	after ?TIMEOUT ->
			
		reflector:sync_to_reflector(?SUBS)
	
	end,	
	loop().


%% Try to start a daemon
%%      Cmd, State, Event
hcevent(_  , _    , {cmd, start}) ->
	Port=base:getport(),
	gevent( {port, Port} );	

%% We've got a valid port... let's try to connect
hcevent(_, run, {port, {port, Port}} ) ->
	put(state, try_connect),
	reflector:send(self(), management_port, Port),
	reflector:send(self(), client, connect),
	ok;


hcevent(_, _, {cmd, stop}) ->
	ok.




