%% Author:      Jean-Lou Dupont
%% Created:     2009-06-28
%% Description: Message Switch support functions
%%
%%
%%

-module(switch).

%%
%% Include files
%%

%% -------------------------------------------------------
%% Exported Functions
%% -------------------------------------------------------

%%
%% API Functions
%%
-export([
		 subscribe/1,
		 subscribe/2,
		 publish/2,
		 publish/3
		 ]).

-export([
		 add_subscriber/3,
		 remove_subscriber/1,
		 add_msgtype/1
		 ]).


%%
%% Local Functions
%%
-export([
		 remove_subscriber_from_type/2,
		 remove_subscriber_from_types/2
		 ]).


%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%      API
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


%%
%% @spec subscribe(Type)
%%
subscribe(Type) ->
	rpc:call({subscribe, undefined, Type}).

%%
%% @spec subscribe(Cb, Type)
%%
subscribe(Cb, Type) ->
	rpc:call({subscribe, Cb, Type}).

%%
%% @spec publish(MsgType, Msg)
%%
publish(MsgType, Msg) ->
	
	ok.

publish(Cb, MsgType, Msg) ->
	
	ok.

%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%   SHOULD ONLY BE USED BY THE SWITCH PROCESS
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			  

add_subscriber(_Client, _Cb, undefined) ->
	ok;

add_subscriber(Client, Cb, Type) when is_atom(Type)->
	base:add_to_list_no_duplicates({msgtype, Type}, Client),
	rpc:handlecb(Client, Cb).



%% Go through all the types list and remove the subscriber
remove_subscriber(Client) ->
	Types=base:getvar(msgtypes, []),
	
	ok.



remove_subscriber_from_types(_Client, []) ->
	ok;
	
remove_subscriber_from_types(Client, Types) ->
	[Htype|Ttype]=Types,
	remove_subscriber_from_type(Client, Htype),
	remove_subscriber_from_types(Client, Ttype).



remove_subscriber_from_type(Client, Type) ->
	List=base:getvar({msgtype, Type}, []),
	NewList=List--[Client],
	put({msgtype, Type}, NewList),
	ok.	




%% Add a message type to the registered list
add_msgtype(MsgType) ->
	base:add_to_list_no_duplicates(msgtypes, MsgType),
	ok.


