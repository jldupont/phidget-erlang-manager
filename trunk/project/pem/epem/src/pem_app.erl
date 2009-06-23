%% Author: Jean-Lou Dupont
%% Created: 2009-06-22
%% Description: TODO: Add description to pem
%%
%% --start
%% --stop
%%
-module(pem_app).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 stop/0,
	 start/0,
	 start/1,
	 start/2,
	 loop/0
        ]).

%% ====================================================================!
%% API functions
%% ====================================================================!
start() ->
	start([]).

start(_,_) ->
	start([]).

start(Args) ->
	base:ilog(?MODULE, "Args[~p]~n", [Args]),
	process_flag(trap_exit,true),
	pem_sup:start_link(Args),
	loop().

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
		stop ->
			exit(ok);
		Other->
			Other
	end,
	loop().
