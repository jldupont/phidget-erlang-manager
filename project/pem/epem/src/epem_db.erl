%% Author: Jean-Lou Dupont
%% Created: 2009-09-13
%% Description: ODBC tools
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

-module(epem_db).

%%
%% Macros
%%
-define(conn_string, "DSN=~s;").

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
		 %open/0, %% for test purposes
		 open/1,
		 close/1,
		 create_tables/1,
		 create_device_table/1,
		 create_event_table/1
		
		,format_timestamp/6
		 ]).

%%
%% API Functions
%%


open(DSN) ->
	odbc:start(),
	ConnString=io_lib:format(?conn_string, [DSN]),
	try odbc:connect(ConnString, []) of
		{ok, Pid} -> {ok, Pid};
		Other     -> Other
	catch X:Y -> {X, Y}
	end.



close(undefined) ->
	ok;

close(Conn) ->
	try	odbc:disconnect(Conn) 
	catch X:Y -> {X,Y}
	end.



create_tables(Conn) ->
	%%io:format("creating device table~n"),
	Ret1 = create_device_table(Conn),
	%%io:format("Result: ~p~n",[Ret1]),
	
	%%io:format("creating event table~n"),
	Ret2 = create_event_table(Conn),
	%%io:format("Result: ~p~n",[Ret2]),
	Ret1 and Ret2.



create_device_table(Conn) ->
	try odbc:sql_query(Conn, ?device_table) of
		{updated, _Result} -> ok;
		Ret                -> Ret
	catch X:Y -> {X,Y}
	end.


create_event_table(Conn) ->
	try odbc:sql_query(Conn, ?event_table) of
		{updated, _Result} -> ok;
		Ret                -> Ret
	catch X:Y -> {X,Y}
	end.



-define(timestamp_template, "~P-~P-~P ~P:~P:~P").

%% Useful for ODBC database access
format_timestamp(Year, Month, Day, Hour, Min, Sec) ->
	L=io_lib:format(?timestamp_template, [Year,4, 
										Month,2, 
										Day,2,
										Hour,2,
										Min,2,
										Sec,2]),
	Ts2=erlang:iolist_to_binary(L),
	erlang:binary_to_list(Ts2).
	


