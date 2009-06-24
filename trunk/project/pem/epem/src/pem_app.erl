%% Author: Jean-Lou Dupont
%% Created: 2009-06-22
%% Description: TODO: Add description to pem
%%
%% start()            -> starts daemon
%% start(debug)       -> starts daemon
%% start(debug,start) -> starts daemon
%% start(debug,stop)  -> stops daemon
%% start(stop)        -> stops daemon
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

-export([
		 start_daemon/1,
		 stop_daemon/1
		 ]).

%% ====================================================================!
%% API functions
%% ====================================================================!
start() ->
	start_daemon([]).

start(debug) ->
	start_daemon([debug]);

start(stop) ->
	stop_daemon([]).

start(debug, start) ->
	start_daemon([debug]);

start(debug, stop) ->
	stop_daemon([debug]).

start_daemon(Args) ->
	base:ilog(?MODULE, "Args[~p]~n", [Args]),
	process_flag(trap_exit,true),
	pem_sup:start_link(Args),
	loop().

stop_daemon(_Args) ->
	ok.

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
