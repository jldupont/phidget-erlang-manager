%% Author: Jean-Lou Dupont
%% Created: 2009-06-03
-module(test).

%%
%% Include files
%%

%%
%% API
%%
-export([start/0, start/1, stop/0]).

%% internal
-export([init/2, loop/1]).

%%
%% Local Functions
%%

start() ->
    start("").

start(Param) ->
    spawn_link(?MODULE, init, ["/usr/local/bin/pem_drv_mng_debug", Param]).

stop() ->
    ?MODULE ! stop.

init(ExtPrg, Param) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg++" "++Param}, [{packet, 2}, binary, exit_status]),
    loop(Port).

loop(Port) ->
    receive
		
		stop ->
			io:format("called [stop]"),
			erlang:port_close(Port),
			exit(normal);
		
		{Port, {data, Data}} ->
			Decoded = binary_to_term(Data),
			io:format("Message! Decoded[~p]~n", [Decoded])
					
    end,
	loop(Port).

