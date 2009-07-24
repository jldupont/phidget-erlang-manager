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
		 loop/0,
		 open_db/0,
		 open_db/3
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
		
		{args, Args} ->
			put(args, Args),
			switch:subscribe(journal, ?SUBS);

		%% Send the 'ready' signal
		{switch, subscribed} ->
			%%base:ilog(?MODULE, "subscribed~n",[]),
			switch:publish(journal, ready, self());

		%% Receive database details
		{db_details, Db, User, Pass} ->
			put(database, Db),
			put(username, User),
			put(password, Pass),
			try_start_db();
			
		stop ->
			exit(ok);
		
		%% ------------------------------
		
		{_From, phidgetdevice, Msg} ->
			handle_pd(Msg);
		
		Other ->
			base:ilog(?MODULE, "received Msg[~p]~n", [Other])

	after 3000 ->
			
			try_start_db()
	
	end,
	loop().

handle_pd(Msg) ->
	{{Serial, Type, Status}, {{Year, Month, Day}, {Hour, Min, Sec}, _}} = Msg,
	 ok.

%% ===============
%% LOCAL FUNCTIONS
%% ===============

try_start_db() ->
	State=base:getvar(state, not_connected),
	
	case State of
		
		connected ->
			ok;
		
		not_connected ->
			open_db()
	
	end,
	ok.


open_db() ->
	Db=  base:getvar(database, undefined),
	User=base:getvar(username, undefined),
	Pass=base:getvar(password, undefined),
	open_db(Db, User, Pass).

open_db(undefined, _, _) ->
	cant_connect;

open_db(_,undefined, _) ->
	cant_connect;

open_db(_, _, undefined) ->
	cant_connect;

open_db(Db, User, Pass) ->
	case db:open(Db, User, Pass) of
		{ok, Conn} ->
			put(db_conn, Conn),
			put(state, connected);
		_ ->
			cant_connect
	end.

