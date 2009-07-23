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
-define(conn_string, "DSN=pem;database=~s;user=~s;password=~s;option=3;").

-define(device_table, "CREATE TABLE IF NOT EXISTS `device` ("
	   "`serial` int(8) NOT NULL, "
	   "`type` varchar(64) NOT NULL, "
	   "`version` varchar(255) NOT NULL, "
	   "`name` varchar(64) NOT NULL, "
	   "`label` varchar(64) NOT NULL, "
	   "`ts` timestamp NOT NULL);").

-define(event_table, "CREATE TABLE IF NOT EXISTS `event` ("
  "`serial` int(8) NOT NULL,"
  "`index` int(2) NOT NULL,"
  "`value` int(8) NOT NULL,"
  "`ts` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP) ").


-define(insert_device_statement, "INSERT INTO device(serial, type, version, name, label, ts)"
	   "VALUES(?, ?, ?, ?, ?, ?)").


-define(insert_event_statement, "INSERT INTO event(serial, index, value, ts)"
	   "VALUES(?, ?, ?, ?)").


%%
%% Exported Functions
%%
-export([
		 open/0, %% for test purposes
		 open/3,
		 create_tables/1,
		 create_device_table/1,
		 create_event_table/1
		 ]).

-export([
		 insert_device_update/1,  %% for test purposes
		 insert_device_update/7,
		 insert_event_update/1,
		 insert_event_update/5
		 ]).
%%
%% API Functions
%%

open() ->
	open("test", "test", "pass").


open(Database, User, Password) ->
	application:start(odbc),
	ConnString=io_lib:format(?conn_string, [Database, User, Password]),
	try odbc:connect(ConnString, []) of
		{ok, Pid} ->
			{ok, Pid};
		Other ->
			Other
	catch
		Error ->
			Error
	end.


create_tables(Conn) ->
	Ret1 = create_device_table(Conn),
	Ret2 = create_event_table(Conn),
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
insert_device_update(Conn) ->
	insert_device_update(Conn, 666, "ifk", "v1.0", "name", "label", "0000-00-00 00:00:00").


insert_device_update(Conn, Serial, Type, Version, Name, Label, Ts) ->
	try odbc:param_query(Conn, ?insert_device_statement, [{sql_integer, [Serial]}, 
														  {{sql_varchar,  64}, [Type]},
														  {{sql_varchar,  64}, [Version]},
														  {{sql_varchar,  64}, [Name]},
														  {{sql_varchar,  64}, [Label]},
														  {{sql_varchar,  20}, [Ts]}]) of
		{updated, _Nbr}
		  -> ok;
		Other ->
			Other
	catch
		Error ->
			Error
	end.


%% Test only
insert_event_update(Conn) ->
	insert_event_update(Conn, 666, 777, 888, "0000-00-00 00:00:00").


insert_event_update(Conn, Serial, Index, Value, Ts) ->
	try odbc:param_query(Conn, ?insert_event_statement, [{sql_integer, [Serial]},
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




%%
%% Local Functions
%%

