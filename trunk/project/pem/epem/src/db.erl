%% Author: Jean-Lou Dupont
%% Created: 2009-07-22
%% Description: TODO: Add description to db
%%
%% Device table
%% ============
%%
%%  serial  type     version   name     label    timestamp
%%  INT     VARCHAR  VARCHAR   VARCHAR  VARCHAR  TIMESTAMP
%%
%% Event table
%% ===========
%%
%%  serial  index  value  timestamp
%%  INT     INT    INT    TIMESTAMP
%%

-module(db).

%%
%% Macros
%%
-define(conn_string, "DSN=~s;").

-define(test_conn_string, "pem").

-define(device_table, "CREATE TABLE IF NOT EXISTS `device` ("
	   "`id` int(11) NOT NULL auto_increment, "
	   "PRIMARY KEY(`id`), "
	   "`status` varchar(16) NOT NULL, "
	   "`serial` int(8) NOT NULL, "
	   "`type` varchar(64) NOT NULL, "
	   "`version` varchar(64) NOT NULL, "
	   "`name` varchar(64) NOT NULL, "
	   "`label` varchar(64) NOT NULL, "
	   "`ts` timestamp NOT NULL)").

-define(event_table, "CREATE TABLE IF NOT EXISTS `event` ("
  "`id` int(11) NOT NULL auto_increment, "
  "PRIMARY KEY(`id`), "
  "`serial` int(8) NOT NULL, "
  "`iotype` varchar(8) NOT NULL, "
  "`idx` int(8) NOT NULL, "
  "`value` int(8) NOT NULL, "
  "`ts` timestamp NOT NULL)").


-define(insert_device_statement, "INSERT INTO device(serial, type, version, name, label, status, ts)"
	   " VALUES(?, ?, ?, ?, ?, ?, ?)").


-define(insert_event_statement, "INSERT INTO event(serial, iotype, idx, value, ts)"
	   " VALUES(?, ?, ?, ?, ?)").


%%
%% Exported Functions
%%
-export([
		 open/0, %% for test purposes
		 open/1,
		 close/1,
		 create_tables/1,
		 create_device_table/1,
		 create_event_table/1
		 ]).

-export([
		 insert_device_update/1,  %% for test purposes
		 insert_device_update/8,
		 insert_event_update/1,
		 insert_event_update/6
		 ]).

-export([
		 test/0
		 ]).
%%
%% API Functions
%%

open() ->
	open(?test_conn_string).


open(DSN) ->
	odbc:start(),
	ConnString=io_lib:format(?conn_string, [DSN]),
	try odbc:connect(ConnString, []) of
		{ok, Pid} ->
			{ok, Pid};
		Other ->
			Other
	catch
		Error ->
			Error
	end.



close(undefined) ->
	ok;

close(Conn) ->
	odbc:disconnect(Conn).



create_tables(Conn) ->
	%%io:format("creating device table~n"),
	Ret1 = create_device_table(Conn),
	%%io:format("Result: ~p~n",[Ret1]),
	
	%%io:format("creating event table~n"),
	Ret2 = create_event_table(Conn),
	%%io:format("Result: ~p~n",[Ret2]),
	
	base:and_ret(Ret1, Ret2).



create_device_table(Conn) ->
	try odbc:sql_query(Conn, ?device_table) of
		{updated, _Result} ->
			ok;

		Ret ->
			Ret
	catch
		_ ->
			error
	end.


create_event_table(Conn) ->
	try odbc:sql_query(Conn, ?event_table) of
		{updated, _Result} ->
			ok;

		Ret ->
			Ret
	catch
		_ ->
			error
	end.


%% Test only
test() ->
	{ok,Conn}=db:open(),
	db:create_tables(Conn),
	io:format("Insert device update~n"),
	insert_device_update(Conn),
	io:format("Insert event update~n"),
	insert_event_update(Conn).


insert_device_update(Conn) ->
	%%insert_device_update(Conn, 666, "ifk", "v1.0", "name", "label", "n/a", "0000-7-00 00:00:00").
	%%insert_device_update(Conn, 666, "ifk", "v1.0", "name", "label", "n/a", ["2009",45,"7",45,"27",32,"11",58,"19",58,"47"]).
	insert_device_update(Conn, 666, "ifk", "v1.0", "name", "label", "n/a", "2009-7-27 11:19:48").

%% @spec(Ref, Serial, Type, Version, Name, Label, State, Ts)
%% INSERT INTO device(serial, type, version, name, label, status, ts)
insert_device_update(Conn, Serial, Type, Version, Name, Label, State, Ts) ->
	%%base:ilog(?MODULE, "Serial[~p] Type[~p] Version[~p], Name[~p] Label[~p] State[~p] Ts[~p~n]",[Serial, Type, Version, Name, Label, State, Ts]),
	try odbc:param_query(Conn, ?insert_device_statement, [{sql_integer, [Serial]}, 
														  {{sql_varchar,  64}, [Type]},
														  {{sql_varchar,  64}, [Version]},
														  {{sql_varchar,  64}, [Name]},
														  {{sql_varchar,  64}, [Label]},
														  {{sql_varchar,  16}, [State]},
														  {{sql_varchar,  20}, [Ts]}]) of
		{updated, _Nbr}
		  -> ok;
		Other ->
			base:elog(?MODULE, "error [~p]~n", [Other]),
			Other
	catch
		Error ->
			base:elog(?MODULE, "error [~p]~n", [Error]),
			Error
	end.


%% Test only
insert_event_update(Conn) ->
	insert_event_update(Conn, 666, "dout", 777, 888, "0000-00-00 00:00:00").


insert_event_update(Conn, Serial, IOType, Index, Value, Ts) ->
	try odbc:param_query(Conn, ?insert_event_statement, [{sql_integer, [Serial]},
														 {{sql_varchar, 8}, [IOType]},
														 {sql_integer, [Index]},
														 {sql_integer, [Value]}, 
														 {{sql_varchar,  20}, [Ts]}]) of
		{updated, _Nbr}
		  -> ok;
		Other ->
			Other
	catch
		Error ->
			Error
	end.

