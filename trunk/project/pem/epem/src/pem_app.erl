%% Author: Jean-Lou Dupont
%% Created: 2009-06-22
%% Description: TODO: Add description to pem
-module(pem_app).

-behaviour(application).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 stop/1,
	 start/0,
	 start/1,
	 start/2,
	 loop/0
        ]).

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start() ->
	start([]).

start(_,_) ->
	start([]).

start(Args) ->
	process_flag(trap_exit,true),
	pem_sup:start_link(Args),
	loop().

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

loop() ->
	
	receive
		Other->
			Other
	end,

	loop().
