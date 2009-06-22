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
start(_,_) ->
	start().

start() ->
	process_flag(trap_exit,true),
	pem_sup:start_link(),
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
