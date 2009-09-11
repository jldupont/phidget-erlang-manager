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
		 cmd/1
		 ]).


%% For Testing...
cmd([ping]) ->
	handle_reply(pong);

cmd([getcmds]) ->
	handle_reply(?RPC:getcmds());


%% @doc Send a command to the daemon
%%		along with some parameters.
%%
cmd([Command|Params]) ->
	Reply=send(cmd, Command, Params),
	handle_reply(Reply);
	

%% @doc Send a command to the daemon
%%		A response is sent through stdout and
%%		the process exit status code is set.
%%
cmd(Cmd) ->
	Reply=send(cmd, Cmd),
	handle_reply(Reply).



handle_reply(Reply) ->
	case Reply of
		{error, Reason} ->
			io:format("{error, ~p}", [Reason]),
			halt(1);
		Response ->
			io:format("{response, ~p}", [Response]),
			halt(0)
	end.
	
			

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

%% @doc Send a message to the daemon along with some parameters
%%
%% @spec send(ReplyContext, Command, Params) -> {error, Reason} | {ReplyContext, Msg}
%% where
%%	Reason = term()
%%	ReplyContext = atom()
%%	Msg = term()
%%	Params = term()
%%
send(ReplyContext, Command, Params) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			?RPC:rpc(ReplyContext, {Command, Params});
		_ ->
			{error, invalid_command}
	end.

