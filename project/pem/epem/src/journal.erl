%% Author: Jean-Lou Dupont
%% Created: 2009-06-19
%% Description:  This module logs all device 
%%               related messages to the database 
%%
%%
%% Messages treated:
%% =================
%%
%%  {phidgetdevice, {Serial,Type, State}}
%%  {device,        {Serial,Type, State, Version, Name, Label}}
%%  {din,           {Serial, Index, Value}}
%%  {dout,          {Serial, Index, Value}}

-module(journal).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 start_link/1,		 
		 stop/0
		 ]).

%%
%% Local Functions
%%
-export([
		 elog/2,
		 ilog/2,
		 loop/0,
		 sync_reflector/0,
		 sync_reflector/2,
		 do_subscriptions/0
		 ]).

%%
%% API Functions
%%
start_link(_Args) ->
	start_link().

start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register( ?MODULE, Pid ),
	ilog("~p: start_link: PID[~p]~n", [?MODULE, Pid]),
	?MODULE ! start,
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
		start ->
			sync_reflector(),
			ok;
		
		stop ->
			exit(ok);
		
		{reflector, subscribe, ok} ->
			ok;
	
		Other ->
			error_logger:info_msg("~p: received Msg[~p]~n", [?MODULE, Other])

	after 5000 ->
		sync_reflector()

	end,
	loop().

sync_reflector() ->
	Current = whereis(reflector),
	Old = get(reflector),
	sync_reflector(Current, Old).

sync_reflector(Current, Old) when Old == Current ->
	ok;

sync_reflector(Current, Old) when Old /= Current ->
	put(reflector, Current),
	do_subscriptions().

do_subscriptions() ->
	try 
		reflector:subscribe(?MODULE, phidgetdevice),
		reflector:subscribe(?MODULE, device),
		reflector:subscribe(?MODULE, din),
		reflector:subscribe(?MODULE, dout)
	catch
		X:Y ->
			error_logger:error_msg("~p: do_subscriptions: ERROR, X[~p] Y[~p]~n", [?MODULE, X, Y])
	end.


