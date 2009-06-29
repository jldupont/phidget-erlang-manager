%% Author: Jean-Lou Dupont
%% Created: 2009-06-28
%% Description: TODO: Add description to switch
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
		 subscribe/2
		 ]).

-export([
		 add_subscriber/3
		 ]).


%%
%% Local Functions
%%
-export([
		rpc/1
		 ]).



%%
%% @spec subscribe(Type)
%%
subscribe(Type) ->
	rpc({subscribe, undefined, Type}).

%%
%% @spec subscribe(Cb, Type)
%%
subscribe(Cb, Type) ->
	rpc({subscribe, Cb, Type}).



rpc({Cmd, Cb, Args}) ->
	Ret = switch ! {self(), Cmd, Cb, Args},
	case Ret of
		{self(), Cmd, Cb, Args} ->
			ok;
		_ ->
			error
	end.


%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%   SHOULD ONLY BE USED BY THE SWITCH PROCESS
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			  
add_subscriber(_Client, _Cb, undefined) ->
	ok;

add_subscriber(Client, Cb, Type) when is_atom(Type)->
	base:add_to_list_no_duplicates({msgtype, Type}, Client).
	
	
	