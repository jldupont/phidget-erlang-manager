%% Author: Jean-Lou Dupont
%% Created: 2009-06-18
%% Description: Publishes messages from a Source
%%              to the target Destination subscriber(s).
%%
%% {subscribe, Msgtype}
%%
%%

-module(reflector).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	init/1,
	start_link/0,
	stop/0,
	subscribe/2,
	unsubscribe/2	 
	]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
		 ilog/2,
		 elog/2,
		 loop/0,
		 rpc/1,
		 publish/1,
		 do_publish/4,
		 add_client/2,
		 add_client/3,
		 remove_client/2
		 ]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
subscribe(Client, Msgtype) ->
	Ret = rpc({subscribe, Client, Msgtype}),
	error_logger:info_msg("~p: subscribe: Client[~p] Msgtype[~p] Ret[~p]~n", [?MODULE, Client, Msgtype, Ret]),
	Ret.

unsubscribe(Client, Msgtype) ->
	Ret = rpc({unsubscribe, Client, Msgtype}),
	error_logger:info_msg("~p: unsubscribe: Client[~p] Msgtype[~p] Ret[~p]~n", [?MODULE, Client, Msgtype, Ret]),
	Ret.

rpc(Q) ->
	Pid = self(),
	try ?MODULE ! {Pid, Q} of
		{Pid, _} ->
			ok;
		Other ->
			Other
	catch
		_:_ ->
			err
	end.

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/0
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register( ?MODULE, Pid ),
	%%error_logger:info_msg("reflector:start_link: PID[~p]~n", [Pid]),
	{ok, Pid}.

init(_) ->
	error_logger:info_msg("reflector: init: STARTING~p"),
	{ok,0}.

%% --------------------------------------------------------------------
%% Func: stop/0
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
	error_logger:error_msg("reflector: STOP CALLED!~n"),
    rpc({stop}).

%% ====================================================================
%% Internal functions
%% ====================================================================

elog(X,Y) ->
	error_logger:error_msg("~p: "++X, [?MODULE|Y]).

ilog(X,Y) ->
	error_logger:info_msg("~p: "++X, [?MODULE|Y]).

%% ==========
%% Func: loop
%% ==========
loop() ->
	receive
		{_From, {stop}} ->
			error_logger:warning_msg("reflector: received STOP~n"),
			exit(self(), ok);

		%% SUBSCRIBE command
		{From, {subscribe, Client, Msgtype}} ->
			add_client(Client, Msgtype),
			
			%provide feedback to caller
			From ! {reflector, subscribe, ok},
			ok;

		%% UNSUBSCRIBE command
		{From, {unsubscribe, Client, Msgtype}} ->
			remove_client(Client, Msgtype),
			From ! {reflector, unsubscribe, ok},
			ok;
		
		%% Message publication
		{_From, {Msgtype, Msg, Timestamp}} ->
			publish({Msgtype, Msg, Timestamp}),
			ok;

		Error ->
			error_logger:warning_msg("reflector:loop: unsupported message, [~p]~n", [Error]),
			Error

	end,
	?MODULE:loop().

%% =======================================================================================
%% SUBSCRIBE/UNSUBSCRIBE API
%% =======================================================================================

add_client(Client, Msgtype) ->
	Liste = get(Msgtype),
	add_client(Liste, Client, Msgtype).

%no Client yet for Msgtype
add_client(undefined, Client, Msgtype) ->
	New_liste = [Client],
	put(Msgtype, New_liste),
	ok;

add_client(Liste, Client, Msgtype) ->
	
	% we do not want duplicates
	Filtered_liste = Liste -- [Client],
	
	New_liste = Filtered_liste ++ [Client],
	put(Msgtype, New_liste),
	ok.

remove_client(undefined, _) ->
	ok;

remove_client(Client, Msgtype) ->
	error_logger:warning_msg("~p: remove_client: Client[~p] Msgtype[~p]~n", [?MODULE, Client, Msgtype]),
	Liste = get(Msgtype),
	Updated = Liste--[Client],
	put(Msgtype, Updated),
	ok.

%% =======================================================================================
%% PUBLISH API
%% =======================================================================================

publish(M) ->
	%%error_logger:info_msg("reflector: publish Msg[~p]~n", [M]),
	{Msgtype, Msg, Timestamp} = M,
	Liste = get(Msgtype),
	do_publish(Liste, Msgtype, Msg, Timestamp).


do_publish([], _Msgtype, _, _) ->
	%%error_logger:warning_msg("reflector:do_publish: NO MORE subscribers for [~p]~n", [Msgtype]),
	ok;


do_publish(undefined, Msgtype, _, _) ->
	error_logger:warning_msg("reflector:do_publish: no subscribers for [~p]~n", [Msgtype]),
	ok;


do_publish(Liste, Msgtype, Msg, Timestamp) ->
	elog("do_publish, Msgtype[~p] liste[~p]~n", [Msgtype, Liste]),
	[Current|Rest] = Liste,
	ilog("publish, TO[~p] Msgtype[~p] Msg[~p]~n", [Current, Msgtype, Msg]),
	
	try	Current ! {Msgtype, Msg, Timestamp} of
		{Msgtype, Msg, Timestamp} ->
			ok;
		Other ->
			error_logger:warning_msg("~p: do_publish: result[~p]~n", [?MODULE, Other]),
			remove_client(Current, Msgtype)
	catch
		X:Y ->
			error_logger:error_msg("~p: do_publish: ERROR sending, X[~p] Y[~p]~n", [?MODULE, X, Y]),
			remove_client(Current, Msgtype)
	end,
	do_publish(Rest, Msgtype, Msg, Timestamp).

