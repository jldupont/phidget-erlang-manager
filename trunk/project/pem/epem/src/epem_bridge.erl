%% Author: Jean-Lou Dupont
%% Created: 2009-09-11
%% Description: RPC bridge for 2way communication between daemon management script and the actual daemon
%%
-module(epem_bridge).

-define(RPC, epem_rpc).
-define(JSON, epem_json).


%%
%% API functions
%%
-export([
		 cmd/1
		
		,encode/1
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
			%io:format("~p", [{error, Reason}]),
			J=json({error, Reason}),
			io:format("~s", [J]),
			halt(1);
		Response ->
			%io:format("~p", [{"response", Response}]),
			J=json({response, Response}),
			io:format("~s", [J]),
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


%% --------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------

json(Term) ->
	E=encode(Term),
	%J=?JSON:obj_to_list(E),
	?JSON:encode({array, E}).


	
encode(Tuple) when is_tuple(Tuple) ->
	encode(erlang:tuple_to_list(Tuple));

encode(List) when is_list(List) ->
	encode(List, []);

encode(What) ->
	{error, not_list}.

encode([], Acc) ->
	Acc;

encode([H|T], Acc) when is_tuple(H) ->
	L=erlang:tuple_to_list(H),
	encode([L|T], Acc);

encode([H|T], Acc) when is_atom(H) ->
	encode(T, Acc++[erlang:atom_to_list(H)]);

encode([H|T], Acc) when is_list(H) ->
	L=encode(H, []),
	encode(T, Acc++[L]);

encode([H|T], Acc) when is_float(H) ->
	encode(T, Acc++[erlang:float_to_list(H)]);

encode([H|T], Acc) when is_integer(H) ->
	encode(T, Acc++[erlang:integer_to_list(H)]).

	
