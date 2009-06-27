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
%% Include files
%%

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
	Pid = spawn(?MODULE, loop, []),
	register(?MODULE, Pid),
	?MODULE ! {cmd, Cmd},
	{ok, Pid}.
	
	

loop() ->
	receive
		stop ->
			exit(ok);
		
		{cmd, Cmd} ->
			io:format("Command[~p]~n", [Cmd])
	
	end,	
	loop().
