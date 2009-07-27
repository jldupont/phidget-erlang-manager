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
		 open_db/0
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
		{db_details, DSN} ->
			%%base:ilog(?MODULE, "received DSN[~s]~n", [DSN]),
			put(dsn, DSN),
			try_start_db();
			
		stop ->
			exit(ok);
		
		%% ------------------------------
		
		{_From, phidgetdevice, Msg} ->
			handle_pd(Msg);

		%%{_From, din, Msg} ->
			%%handle_io("din", Msg);
		
		%%{_From, dout, Msg} ->
			%%handle_io("dout", Msg);
		
		_Other ->
			ok
			%%base:ilog(?MODULE, "received Msg[~p]~n", [Other])

	%% msec
	after 3000 ->
			
			try_start_db()
	
	end,
	loop().

handle_pd(Msg) ->
	{{Serial, Type, Status}, {{Year, Month, Day}, {Hour, Min, Sec}, _}} = Msg,
	Ts=base:format_timestamp(Year, Month, Day, Hour, Min, Sec),
	base:ilog(?MODULE, "pd: Serial[~p] Type[~p] Status[~p]~n",[Serial, Type, atom_to_list(Status)]),
	Conn = base:getvar(db_conn, undefined),
	handle_pd(Conn, Serial, Type, atom_to_list(Status), Ts).


handle_pd(undefined, _Serial, _Type, _Status, _Ts) ->
	base:elog(?MODULE, "no db connection~n",[]),
	db_conn_err;
	
	
handle_pd(Conn, Serial, Type, Status, Ts) when is_pid(Conn) ->
	base:ilog(?MODULE, "inserting pd: Serial[~p] Type[~p] Status[~p]~n", [Serial, Type, Status]),
	%%insert_device_update(Conn, Serial, Type, Version, Name, Label, State, Ts) ->
	db:insert_device_update(Conn, Serial, Type, " ",     " ",   " ",    Status, Ts);

handle_pd(_Conn, _Serial, _Type, _Status, _Ts) ->
	base:elog(?MODULE, "invalid db connection~n",[]).



handle_io(IOType, Msg) ->
	%%io:format("handle_io: msg:  ~p~n", [Msg]),
	{{Serial, Index, Value}, {{Year, Month, Day}, {Hour, Min, Sec}, _MegaSecs}}=Msg,
	Ts=base:format_timestamp(Year, Month, Day, Hour, Min, Sec),
	base:ilog(?MODULE, "io: Serial[~p] IOType[~p] Index[~p] Value[~p]~n", [Serial, IOType, Index, Value]),
	Conn = base:getvar(db_conn, undefined),
	handle_io(IOType, Conn, Serial, Index, Value, Ts).

handle_io(_IOType, undefined, _Serial, _Index, _Value, _Ts) ->
	base:elog(?MODULE, "no db connection~n",[]),
	db_conn_err;

handle_io(IOType, Conn, Serial, Index, Value, Ts) when is_pid(Conn) ->
	base:ilog(?MODULE, "inserting event~n", []),
	db:insert_event_update(Conn, Serial, IOType, Index, Value, Ts);

handle_io(_IOType, _Conn, _Serial, _Index, _Value, _Ts) ->
	base:elog(?MODULE, "invalid db connection~n",[]).


%% ===============
%% LOCAL FUNCTIONS
%% ===============

try_start_db() ->
	ConnState=base:getvar(db_conn, undefined),
	
	case ConnState of
		
		undefined ->
			case open_db() of
				connected ->
					connected;
					%%base:ilog(?MODULE, "db connect~n",[]);
			
				_ ->
					%%base:elog(?MODULE, "CANNOT CONNECT~n",[]),
					not_connected
				
			end;

		_ ->
			  ok
	end.


open_db() ->
	DSN=  base:getvar(dsn, undefined),
	open_db(DSN).

open_db(undefined) ->
	cant_connect;

open_db(DSN) ->
	case db:open(DSN) of
		{ok, Conn} ->
			base:ilog(?MODULE, "connected [~p]~n", [Conn]),
			put(db_conn, Conn),
			db:create_tables(Conn),
			connected;
		_ ->
			put(db_conn, undefined),
			cant_connect
	end.

