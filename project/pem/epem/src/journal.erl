%% Author: Jean-Lou Dupont
%% Created: 2009-06-19
%% Description:  This module logs all device 
%%               related messages to the database 
%%

-module(journal).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 stop/0
		 ]).

%%
%% Local Functions
%%
-export([
		 elog/2,
		 ilog/2,
		 loop/0
		 ]).

%%
%% API Functions
%%
start_link() ->
	Pid = spawn(fun() -> loop() end),
	register( ?MODULE, Pid ),
	ilog("start_link: PID[~p]~n", [?MODULE, Pid]),
	{ok, Pid}.

stop() ->
	?MODULE ! stop.


%%
%% Helper Functions
%% ================
elog(X,Y) ->
	error_logger:error_msg("~p: "++X, [?MODULE|Y]).

ilog(X,Y) ->
	error_logger:info_msg("~p: "++X, [?MODULE|Y]).



%% =========
%% MAIN LOOP
%% =========
loop() ->
	receive
		stop ->
			exit(ok)
	
		
	end,
	loop().
