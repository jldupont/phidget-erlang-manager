%% Author: Jean-Lou Dupont
%% Created: 2009-09-11
%% Description: 
-module(epem_bridge).

-define(RPC, epem_rpc).


%%
%% API functions
%%
-export([
		 send/2
		 ]).


send(ReplyContext, Command) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			?RPC:rpc(ReplyContext, Command);
		_ ->
			{error, invalid_command}
	end.

