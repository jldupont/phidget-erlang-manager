%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Module for communicating with the daemon
%%
%% SUBSCRIPTIONS:
%% ==============
%%
%% {to_daemon,       {MsgId, Msg}}
%% {client,          doconnect}
%%
%%
%% MESSAGE GENERATED:
%% ==================
%% {from_daemon, Message}           Message from Server side
%% {management,  open}              Socket is open & ready
%% {management,  closed}            Socket is closed
%% {management,  error}             Socket cannot be opened
%% {management,  {txerror, MsgId}}  Error transmitting message to Server
%% {management,  {txok,    MsgId}}  Transmission to Server OK
%%
%% {ready, {}}

-module(daemon_client).

%%
%% MACROS
%%
-define(TIMEOUT, 2000).

-define(SUBS, [to_daemon, client]).

%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 start_link/1,
		 stop/0
		 ]).

-export([
		 loop_connection/0,
		 close_socket/0,
		 close_socket/1,
		 send_to_server/2,
		 send_to_server/3
		 ]).

%% ======================================================
%% API Functions
%% ======================================================

start_link()->
	Pid = spawn_link(?MODULE, loop_connection, []),
	register(daemon_client, Pid),
	?MODULE ! {args, []},
	%%base:ilog(?MODULE,"Pid[~p]~n",[Pid]),
	{ok, Pid}.

%% Once started, sends Msg to Recipient
start_link(Args) ->
	Pid = spawn_link(?MODULE, loop_connection, []),
	register(daemon_client, Pid),
	?MODULE ! {args, Args},
	%%base:ilog(?MODULE,"Pid[~p]~n",[Pid]),
	{ok, Pid}.

stop() ->
	daemon_client ! stop.



%% ======================================================
%% Local Functions
%% ======================================================

loop_connection() ->
	receive
		
		%% When we need to notify a root proc of our progress
		{args, Args} ->
			put(args, Args),
			switch:subscribe(daemon_client, ?SUBS);			
		
		{switch, subscribed} ->
			switch:publish(?MODULE, ready, {});			
				
		stop ->
			close_socket(),
			exit(ok);
		
		%% Starts a connection towards the Server
		{_From, client, {doconnect, Port}} ->
			%%io:format("doconnect~n"),
			close_socket(),
			{Code, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, 2}], ?TIMEOUT),
			case Code of
				ok ->
					put(socket, Socket),
					switch:publish(?MODULE, management, open);
				_ ->
					put(socket, undefined),
					switch:publish(?MODULE, management, error)					
			end;

		{to_daemon, {MsgId, Msg}} ->
			send_to_server(MsgId, Msg);
			
		%% From socket
		{tcp, _Sock, Data} ->
			Message = binary_to_term(Data),
			%%io:format("received: ~p~n",[Message]),
			switch:publish(?MODULE, from_daemon, Message);

		%% From socket
		{tcp_closed, _Sock} ->
			put(socket, undefined),
			switch:publish(?MODULE, management, closed)
	
		
	end,
	loop_connection().



close_socket() ->
	Socket=get(socket),
	close_socket(Socket).

close_socket(undefined) ->
	ok;

close_socket(Socket) ->
	gen_tcp:close(Socket),
	put(socket, undefined).





%% Send a message over the socket connection
%% to the server side. The MsgId field is used
%% to correlate a status message back to the
%% subscriber of this Client interface.
send_to_server(MsgId, Msg) ->
	Socket=get(socket),
	send_to_server(Socket, MsgId, Msg).
		
send_to_server(undefined, MsgId, _Msg) ->
	switch:publish(?MODULE, management, {txerror, MsgId});


send_to_server(Socket, MsgId, Msg) ->
	Coded = term_to_binary(Msg),
	case gen_tcp:send(Socket, Coded) of
		ok ->
			switch:publish(?MODULE, management, {txok, MsgId});

		{error,Reason} ->
			base:elog(?MODULE, "send_to_server: ERROR, Reason[~p], MsgId[~p] Msg[~p]~n", [Reason, MsgId, Msg]),
			switch:publish(?MODULE, management, {txerror, MsgId})			

	end.


