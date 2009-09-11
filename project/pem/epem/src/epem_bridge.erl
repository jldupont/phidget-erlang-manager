%% Author: Jean-Lou Dupont
%% Created: 2009-09-11
%% Description: RPC bridge for 2way communication between daemon management script and the actual daemon
%%
-module(epem_bridge).

-define(RPC, epem_rpc).


%%
%% API functions
%%
-export([
		 send/2
		 ]).

%% @doc Send a message to the daemon
%%
%% @spec send(ReplyContext, Command) -> {error, Reason} | {ReplyContext, Msg}
%% where
%%	Reason = term()
%%	ReplyContext = atom()
%%	Msg = term()
%%
send(ReplyContext, Command) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			?RPC:rpc(ReplyContext, Command);
		_ ->
			{error, invalid_command}
	end.

