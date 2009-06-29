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

-define(SUBS, [phidgetdevice, device, din, dout]).

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
		 loop/0
		 ]).

%%
%% API Functions
%%

start_link() ->
	start_link([]).


start_link(Args) ->
	Pid = spawn_link(?MODULE, loop, []),
	register( ?MODULE, Pid ),
	base:ilog(?MODULE, "start_link: PID[~p]~n", [Pid]),
	?MODULE ! {args, Args},
	{ok, Pid}.


stop() ->
	?MODULE ! stop.



%% =========
%% MAIN LOOP
%% =========
loop() ->
	receive
		
		%% Send the 'ready' signal
		{args, Args} ->
			put(args, Args),
			switch:subscribe(journal, ?SUBS);
		
		{switch, subscribed} ->
			switch:publish(journal, ready, self());

			
		stop ->
			exit(ok);
		
		Other ->
			base:ilog(?MODULE, "received Msg[~p]~n", [Other])

	end,
	loop().

