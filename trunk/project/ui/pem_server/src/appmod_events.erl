%% Author: Jean-Lou Dupont
%% Created: 2009-07-28
%% Description: 
-module(appmod_events).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 out/1
		 ]).

-include("yaws.hrl").
-include("yaws_api.hrl").

%%
%% API Functions
%%
out(Args) ->
	%%TimeOut=random:uniform(9),
	PQ = yaws_api:parse_query(Args),
	{_, T} = kfind("timeout", PQ, "5"),
	{TO, Rest} = string:to_integer(T),
	timer:sleep(TO*1000),
	{html, io_lib:format("Timeout: ~p T: ~p  Query: ~p",[TO, T, PQ])}.


%%
%% Local Functions
%%

kfind(_Key, []) ->
	error;

%% Searches through a list for a Key
kfind(Key, List) ->
	case erlang:is_builtin(lists, keyfind, 3) of
		true  ->
			lists:keyfind(Key,1,List);
		false ->
			case lists:keysearch(Key,1,List) of
				{value, Value} ->
					Value;
				_ ->
					false
			end
	end.
	
kfind(Key, [], Default) ->
	{Key, Default};

kfind(Key, List, Default) ->
	Ret=kfind(Key, List),
	case Ret of
		false ->
			{Key, Default};
		{Key, Value} ->
			{Key, Value}
	end.
