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
-define(device_table, "CREATE TABLE IF NOT EXISTS `device` ("
	   "`serial` int(8) NOT NULL, `type` varchar(64) NOT NULL, "
	   "`version` varchar(255) NOT NULL, "
	   "`name` varchar(64) NOT NULL, "
	   "`label` varchar(64) NOT NULL, "
	   "`ts` timestamp NOT NULL);").

-define(event_table, "CREATE TABLE IF NOT EXISTS `event` ("
  "`serial` int(8) NOT NULL,"
  "`index` int(2) NOT NULL,"
  "`value` int(8) NOT NULL,"
  "`ts` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP) ").

%%
%% Exported Functions
%%
-export([
		 open/0, %% for test purposes
		 open/3,
		 create_tables/0,
		 create_device_table/0,
		 create_event_table/0
		 ]).

-export([
		 insert_device_update/5,
		 insert_event/4
		 ]).
%%
%% API Functions
%%

open() ->
	open("test", "test", "pass").

%% Returns code not really necessary but the connection pool Id is
%% We assume 'p1' throughout
open(Database, User, Password) ->
	try mysql:start_link(p1, "localhost", User, Password, Database) of
		{ok, Pid} ->
			{ok, Pid};
		_ ->
			error
	catch
		_ ->
			error
	end,
	ok.


create_tables() ->
	Ret1 = create_device_table(),
	Ret2 = create_event_table(),
	base:and_ret(Ret1, Ret2).



create_device_table() ->
	try mysql:fetch(p1, <<?device_table>>) of
		{updated, _Result} ->
			ok;

		Ret ->
			Ret
	catch
		_ ->
			error
	end.


create_event_table() ->
	try mysql:fetch(p1, <<?event_table>>) of
		{updated, _Result} ->
			ok;

		Ret ->
			Ret
	catch
		_ ->
			error
	end.




insert_device_update(Serial, Type, Name, Label, Ts) ->
	ok.



insert_event(Serial, Index, Value, Ts) ->
	ok.



%%
%% Local Functions
%%

