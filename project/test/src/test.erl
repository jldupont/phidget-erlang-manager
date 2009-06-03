%% Author: Jean-Lou Dupont
%% Created: 2009-06-03
%% Description: TODO: Add description to test
-module(test).

%%
%% Include files
%%

%%
%% API
%%
-export([start/0, start/1, stop/0]).

%% internal
-export([init/1]).

%%
%% Local Functions
%%

start() ->
    start("/usr/local/bin/pem_drv_mng_debug").

start(ExtPrg) ->
    spawn_link(?MODULE, init, [ExtPrg]).

stop() ->
    ?MODULE ! stop.

init(ExtPrg) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
    loop(Port).

loop(Port) ->
    receive
		stop ->
			io:format("called [stop]"),
			erlang:port_close(Port),
			exit(normal);
		Result ->
			io:format("message ~p~n]", [Result]),
			Result
    end,
	loop(Port).

