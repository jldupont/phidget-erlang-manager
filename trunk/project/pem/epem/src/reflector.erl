%% Author: Jean-Lou Dupont
%% Created: 2009-06-18
%% Description: Publishes messages from a Source
%%              to the target Destination subscriber(s).
%%
%% API:
%% ====
%%
%%   - subscribe(Client,   MsgType)
%%   - unsubscribe(Client, MsgType)
%%   - send(From, MsgType, Msg)
%%
%% MESSAGE FORMAT:
%% ===============
%%
%% The format of the message sent to subscribers:
%%
%%  { MsgType, Msg } 

-module(reflector).

%% --------------------------------------------------------------------
%% API exports
%% --------------------------------------------------------------------
-export([
	start/0,
	start_link/0,
	stop/0,
	subscribe/2,
	unsubscribe/2,
	send/3 
	]).

%% SEND / SYNC API
-export([
		send_sync/4,
		sync_to_reflector/1
		 ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
		 loop/0,
		 rpc/1,
		 spublish/1,
		 spublish/2,		 
		 add_client/2,
		 add_client/3,
		 remove_client/2,
		 ssend/2
		 ]).

%% SYNC API
-export([
		 sync_to_reflector/3,
		 log_reflector_error/0
		 ]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

%% @spec subscribe(Client, Msgtype) -> ok || error
%% where 
%%       Client  = atom(),
%%       Msgtype = atom()
subscribe(Client, Msgtype) when is_atom(Msgtype) ->
	Ret = rpc({subscribe, Client, Msgtype}),
	base:ilog(?MODULE, "subscribe: Client[~p] Msgtype[~p] Ret[~p]~n", [Client, Msgtype, Ret]),
	Ret;

%% @spec subscribe(Client, Subs::List) -> ok || error
%% where 
%%       List = [Item],
%%       Item = atom()
subscribe(Client, Subs) ->
	Ret = rpc({subscribe, Client, Subs}),
	base:ilog(?MODULE, "subscribe: Client[~p] Subs[~p] Ret[~p]~n", [Client, Subs, Ret]),
	Ret.


unsubscribe(Client, Msgtype) ->
	Ret = rpc({unsubscribe, Client, Msgtype}),
	base:elog(?MODULE, "unsubscribe: Client[~p] Msgtype[~p] Ret[~p]~n", [Client, Msgtype, Ret]),
	Ret.

%% Send a message on the Reflector
send(From, Msgtype, Msg) when is_atom(From), is_atom(Msgtype) ->
	case ?MODULE ! {From, {send, Msgtype, Msg}} of
		{From, Msgtype, Msg} ->
			ok;
		_ ->
			error
	end.


%% ====================================================================!
%% API functions
%% ====================================================================!
start() ->
	Pid = spawn(?MODULE, loop, []),
	register( ?MODULE, Pid ),
	{ok, Pid}.

start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register( ?MODULE, Pid ),
	{ok, Pid}.

stop() ->
	base:elog(?MODULE, "STOP CALLED!~n"),
    rpc({stop}).


%% =======================================================================================
%% SYNC API
%% 
%%  Used by processes to keep synchronized with the Reflector.
%%  Useful when processes die/restart
%% =======================================================================================


%% Subs = subscriptions
%% Subs = Atom() || [atom, atom, ...]
sync_to_reflector(Subs) ->
	Old=get(reflector_pid),
	Reflector = whereis(reflector),
	sync_to_reflector(Old, Reflector, Subs).

%% Cannot find the reflector now!
sync_to_reflector(_, undefined, _) ->
	log_reflector_error(),
	error;

%% Not much todo -- steady state
sync_to_reflector(Old, Current, _Subs) when Old == Current ->
	ok;

sync_to_reflector(Old, Current, Subs) when Old /= Current ->
	put(reflector_pid, Current),
	subscribe(self(), Subs).
	

log_reflector_error() ->
	Count = base:pvadd(sync_error, 1),
	if
		Count < 5 ->
			base:elog(?MODULE, "reflector NOT found~n");
	
		Count > 5 ->
			base:cond_elog(0.1, ?MODULE, "reflector NOT found~n")
	end.

%% =======================================================================================
%% Send & Sync API
%%
%%  Send messages on the Reflector
%%  and synchronize on failure
%%
%%  Should only be called from other processes.
%% =======================================================================================

send_sync(From, MsgType, Msg, Subs) ->
	Ret = send(From, MsgType, Msg),
	case Ret of
		ok ->
			ok;

		%% Possibly out-of-sync
		error ->
			SRet = subscribe(From, Subs),
			case SRet of
				
				%% We have re-synch'ed
				ok ->
					%% One last chance
					send(From, MsgType, Msg);
			
				_ ->
					error
			end
	end.



%% ==========
%% Func: loop
%% ==========
loop() ->
	receive
		{_From, {stop}} ->
			base:elog(?MODULE, "received STOP~n"),
			exit(self(), ok);

		%% SUBSCRIBE command
		{_From, {subscribe, Client, X}} ->
			add_client(Client, X);
			
		%% UNSUBSCRIBE command
		{_From, {unsubscribe, Client, Msgtype}} ->
			remove_client(Client, Msgtype);
		
		%% Message publication
		{From, {send, Msgtype, Msg}} ->
			spublish({From, Msgtype, Msg});
		
		Error ->
			base:elog(?MODULE, "unsupported message, [~p]~n", [Error]),
			Error

	end,
	?MODULE:loop().

%% =======================================================================================
%% SUBSCRIBE/UNSUBSCRIBE HELPERS
%% =======================================================================================

add_client(_Client, []) ->
	ok;

add_client(Client, Msgtype) when is_atom(Msgtype) ->
	Liste = get({msgtype,Msgtype}),
	add_client(Liste, Client, Msgtype);

add_client(Client, Subs) ->
	[Msgtype|Rest] = Subs,
	add_client(Client, Msgtype),
	add_client(Client, Rest).

%no Client yet for Msgtype
add_client(undefined, Client, Msgtype) when is_atom(Msgtype) ->
	New_liste = [Client],
	put({msgtype,Msgtype}, New_liste),
	ok;

add_client(Liste, Client, Msgtype) when is_atom(Msgtype) ->
	
	% we do not want duplicates
	Filtered_liste = Liste -- [Client],
	
	New_liste = Filtered_liste ++ [Client],
	put({msgtype, Msgtype}, New_liste),
	ok.



remove_client(undefined, _) ->
	ok;

remove_client(Client, Msgtype) ->
	base:ilog(?MODULE, "remove_client: Client[~p] Msgtype[~p]~n", [Client, Msgtype]),
	Liste = get(Msgtype),
	Updated = Liste--[Client],
	put({msgtype, Msgtype}, Updated),
	ok.

%% =======================================================================================
%% PUBLISH HELPERS
%% =======================================================================================

spublish(M) ->
	{From, MsgType, Msg} = M,
	base:ilog(?MODULE, "spublish: From[~p] MsgType[~p] Msg[~p]~n", [From, MsgType, Msg]),	
	To = get({msgtype, MsgType}),
	spublish(To, M).

%% No more subscribers
spublish([], _M) ->
	ok;

%% No subscribers
spublish(undefined, _M) ->
	%%{_From, MsgType, _Msg} = M,
	%%base:ilog(?MODULE, "spublish: no subscribers for [~p]~n", [MsgType]),
	ok;

spublish(To, M) ->
	{From, MsgType, Msg} = M,
	[Current|Rest] = To,
	
	base:ilog(?MODULE, "spublish: Sending From[~p] To[~p] MsgType[~p]~n", [From, To, MsgType]),
	
	ssend(Current, {MsgType, Msg}),
	spublish(Rest, M).


%% Actual point of transmission
ssend(To, {MsgType, Msg}) ->
	
	try	To ! {MsgType, Msg} of
		{MsgType, Msg} ->
			ok;
		Other ->
			base:elog(?MODULE,"ssend: result[~p]~n", [Other]),
			remove_client(To, MsgType)
	catch
		X:Y ->
			base:elog(?MODULE, "ssend: ERROR sending, X[~p] Y[~p]~n", [X, Y]),
			remove_client(To, MsgType)
	end.




rpc(Q) ->
	Pid = self(),
	try ?MODULE ! {Pid, Q} of
		{Pid, _} ->
			ok;
		Other ->
			Other
	catch
		X:Y ->
			base:elog(?MODULE, "rpc error: [~p][~p]~n",[X,Y]),
			error
	end.
