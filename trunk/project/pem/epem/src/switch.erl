%% Author:      Jean-Lou Dupont
%% Created:     2009-06-28
%% Description: Message Switch support functions
%%
%%
%%

-module(switch).

%%
%% API Functions
%%
-export([
		 subscribe/2,
		 publish/3
		 ]).

-export([
		 add_subscriber/2
		 ]).


%%
%% Local Functions
%%
-export([
		 to_switch/3,
		 add_type/1,
		 do_publish_list/5,
		 do_publish/3
		 ]).


%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%      API
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


%%
%% @spec subscribe(Type)
%%
subscribe(From, Type) when is_atom(From) ->
	to_switch(From, subscribe, Type).



%%
%% @spec publish(From, MsgType, Msg)
%%
publish(From, MsgType, Msg) when is_atom(From), is_atom(MsgType) ->
	to_switch(From, publish, {MsgType, Msg}).


%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%   INTERNAL
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

to_switch(From, Cmd, Msg) ->
	try switch ! {From, Cmd, Msg} of
		{From, Cmd, Msg} ->
			ok;
		Error ->
			base:elog(?MODULE, "to_switch: ERROR, From[~p] Cmd[~p] Msg[~p] ERROR[~p]~n", [From, Cmd, Msg, Error]),
			error
	catch
		X:Y ->
			base:elog(?MODULE, "to_switch: EXCEPTION, From[~p] Cmd[~p] From[~p] MsgType[~p] Msg[~p] X[~p] Y[~p]~n", [From, Cmd, Msg, X, Y]),
			error
	end.



%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%   SHOULD ONLY BE USED BY THE SWITCH PROCESS
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			  

add_subscriber(Client, undefined) when is_atom(Client) ->
	base:elog(?MODULE, "add_subscriber: invalid type~n",[]),
	ok;

%%
%% @spec add_subscriber(Client, Type)
%%       Client = Atom()
%%       Type   = Atom()
add_subscriber(Client, Type) when is_atom(Type), is_atom(Client) ->
	base:add_to_list_no_duplicates({msgtype, Type}, Client),
	base:add_type(Type);

add_subscriber(Client, []) when is_atom(Client) ->
	ok;

add_subscriber(Client, TypeList) when is_atom(Client), is_list(TypeList) ->
	[H|T] = TypeList,
	add_subscriber(Client, H),
	add_subscriber(Client, T).



%% Adds a Type to the registered MsgTypes list, no duplicates
add_type(Type) ->
	base:add_to_list_no_duplicates(msgtypes, Type),
	ok.




do_publish(From, MsgType, Msg) when is_atom(From), is_atom(MsgType) ->
	ToList = base:getvar({msgtype, MsgType}, []),
	[To|Rest] = ToList,
	do_publish_list(To, Rest, From, MsgType, Msg).




do_publish_list([], [], From, MsgType, _Msg) when is_atom(From), is_atom(MsgType) ->
	ok;											  
														
do_publish_list(CurrentTo, RestTo, From, MsgType, Msg) when is_atom(From), is_atom(MsgType) ->
	try CurrentTo ! {From, MsgType, Msg} of
		{From, MsgType, Msg} ->
			ok;
		Error ->
			base:elog(?MODULE, "do_publish: Error[~p], From[~p] MsgType[~p] Msg[~p]~n", [Error, From, MsgType, Msg])
	catch
		X:Y ->
			base:elog(?MODULE, "do_publish: EXCEPTION, From[~p] MsgType[~p] Msg[~p] X[~p] Y[~p]~n", [From, MsgType, Msg, X, Y])
	end,
	[NewTo|NewRest] = RestTo,
	do_publish_list(NewTo, NewRest, From, MsgType, Msg).


