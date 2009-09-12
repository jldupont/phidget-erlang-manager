%% Author: Jean-Lou Dupont
%% Created: 2009-09-11
%% Description: RPC bridge for 2way communication between daemon management script and the actual daemon
%%
-module(epem_bridge).

-define(DAEMON_RPC,  epem_rpc).
-define(DAEMON_NODE, epem).
-define(RPC, epem_rpc).
-define(JSON, epem_json).

-define(DAEMON, "erl +d -pa ebin -sname epem -detached -run epem_app start").

%%
%% API functions
%%
-export([
		 cmd/1
		 ]).

cmd([stop]) ->
	Reply=send(pid, pid),
	case Reply of
		{pid, Pid} ->
			StrPid=erlang:integer_to_list(Pid),
			os:cmd("kill -9 "++StrPid),
			handle_reply(stop, stop_sent);
		Other ->
			handle_reply(stop, Other)
	end;

cmd([start]) ->
	Reply=send(pid, pid),
	case Reply of
		{pid, _Pid} ->
			handle_reply(start, daemon_running);
		_ ->
			os:cmd(?DAEMON),
			handle_reply(start, start_issued)
	end;
	


%% For Testing...
cmd([ping]) ->
	handle_reply(ping, pong);

cmd([getcmds]) ->
	handle_reply(getcmds, ?RPC:getcmds());

cmd([tstatus]) ->
	{Pid, _}=string:to_integer(os:getpid()),
	handle_reply(tstatus, Pid );


%% @doc Send a command to the daemon
%%		along with some parameters.
%%
cmd([Command|Params]) ->
	Reply=send(Command, Command, Params),
	handle_reply(Command, Reply);
	

%% @doc Send a command to the daemon
%%		A response is sent through stdout and
%%		the process exit status code is set.
%%
cmd(Cmd) ->
	Reply=send(Cmd, Cmd),
	handle_reply(Cmd, Reply).



handle_reply(Cmd, Reply) ->
	case Reply of
		{error, Reason} ->
			%io:format("~p", [{error, Reason}]),
			JR=json({error, Reason}),
			case JR of
				{error, Reason} ->
					io:format("{error, ~p}", [Reason]),
					halt(1);
				{ok, J} ->
					io:format("~s", [J]),
					halt(1)
			end;
		Response ->
			%io:format("~p", [{"response", Response}]),
			JR=json({{command, Cmd}, {response, Response}}),
			case JR of
				{error, _Reason} ->
					io:format("{error, ~p}", [Response]),
					halt(1);
				{ok, J} ->
					io:format("~s", [J]),
					halt(0)
			end
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
	send(ReplyContext, Command, []).


%% @doc Send a message to the daemon along with some parameters
%%
%% @spec send(ReplyContext, Command, Params) -> {error, Reason} | {ReplyContext, Msg}
%% where
%%	Reason = term()
%%	ReplyContext = atom()
%%	Msg = term()
%%	Params = term()
%%
send(ReplyContext, Command, []) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			dorpc(ReplyContext, Command);
		_ ->
			{error, invalid_command}
	end;

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
			dorpc(ReplyContext, {Command, Params});
		_ ->
			{error, invalid_command}
	end.



%% @doc Function that actually performs the RPC call
%%		to the daemon node... assuming it is running.
%%
dorpc(ReplyContext, Message) ->
	Node=make_node(?DAEMON_NODE),
	
	%io:format("dorpc: command: ~p~n", [Message]),
	case rpc:call(Node, ?DAEMON_RPC, rpc, [ReplyContext, Message], 2000) of
		{badrpc, _Reason} ->
			{error, daemon_not_found};
		
		{ReplyContext, Response}->
			{ReplyContext, Response};
		
		{error, Reason} ->
			{error, Reason};
		
		Other ->
			{error, {unexpected_msg, Other}}
	end.

	

%% --------------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------------

%% @doc JSON encodes an Erlang term
%%
%%
json(Term) ->
	try
		E=encode(Term),
		{ok, ?JSON:encode({array, E})}
	catch 
		_:_ ->
		{error, json_encoding_failed}
	end.



%% @doc Encodes an Erlang term to a
%%		form compatible with the JSON encoder.
%%
%%		Tuple() -> List()
%%		Atom()  -> String()
%%		List()  -> {array, List()}
%%		
encode(Tuple) when is_tuple(Tuple) ->
	encode(erlang:tuple_to_list(Tuple));

encode(List) when is_list(List) ->
	encode(List, []);

encode(Atom) when is_atom(Atom) ->
	erlang:atom_to_list(Atom);
	
encode(Int) when is_integer(Int) ->
	Int;

encode(Float) when is_float(Float) ->
	Float;

encode(What) ->
	{error, {unsupported_term, What}}.

encode([], Acc) ->
	Acc;

encode([H|T], Acc) when is_tuple(H) ->
	L=erlang:tuple_to_list(H),
	encode([L|T], Acc);

encode([H|T], Acc) when is_atom(H) ->
	encode(T, Acc++[erlang:atom_to_list(H)]);

encode([H|T], Acc) when is_list(H) ->
	L=encode(H, []),
	encode(T, Acc++[{array, L}]);

encode([H|T], Acc) when is_float(H) ->
	encode(T, Acc++[H]);

encode([H|T], Acc) when is_integer(H) ->
	encode(T, Acc++[H]).

	

extract_host(Node) when is_atom(Node) -> 
	extract_host(atom_to_list(Node));

extract_host(Node) when is_list(Node) ->
	Tokens = string:tokens(Node, "@"),
	lists:last(Tokens).
	


make_node(Name) ->
	make_node(Name, node()).

make_node(Name, Node) when is_atom(Name) ->
	make_node(erlang:atom_to_list(Name), Node);

make_node(Name , Node) when is_list(Name) ->
	Host=extract_host(Node),
	PartialName=string:concat(Name, "@"),
	CompleteName=string:concat(PartialName, Host),
	erlang:list_to_atom(CompleteName).

