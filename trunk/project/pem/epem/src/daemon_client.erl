%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Module for communicating with the daemon
%%
%% SUBSCRIPTIONS:
%% ==============
%%
%% {to_daemon, {MsgId, Msg}}
%%
%% MESSAGE GENERATED:
%% ==================
%% {from_daemon, Message}           Message from Server side
%% {management,  open}              Socket is open & ready
%% {management,  closed}            Socket is closed
%% {management,  error}             Socket cannot be opened
%% {management,  {txerror, MsgId}}  Error transmitting message to Server
%% {management,  {txok,    MsgId}}  Transmission to Server OK

-module(daemon_client).

%%
%% MACROS
%%
-define(TIMEOUT, 2000).

-define(SUBS, [to_daemon]).

%%
%% Exported Functions
%%
-export([
		 start/1,
		 stop/0
		 ]).

-export([
		 loop_connection/1,
		 close_socket/0,
		 close_socket/1,
		 send_to_reflector/1,
		 send_to_server/2,
		 send_to_server/3
		 ]).

%% ======================================================
%% API Functions
%% ======================================================

start(Port)->
	P = spawn(?MODULE, loop_connection, [Port]),
	register(daemon_client, P),
	{ok, P}.


stop() ->
	daemon_client ! stop.



%% ======================================================
%% Local Functions
%% ======================================================

loop_connection(Port) ->
	receive
		stop ->
			close_socket(),
			exit(ok);
		
		%% Starts a connection towards the Server
		{doconnect} ->
			close_socket(),
			{Code, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, 2}], ?TIMEOUT),
			case Code of
				ok ->
					put(socket, Socket),
					reflector:send_sync(daemon_client, management, open, ?SUBS);
				_ ->
					put(socket, undefined),
					reflector:send_sync(daemon_client, management, error, ?SUBS)
			end;

		{to_daemon, {MsgId, Msg}} ->
			send_to_server(MsgId, Msg);
			
		%% From socket
		{tcp, _Sock, Data} ->
			Message = binary_to_term(Data),
			send_to_reflector(Message);

		%% From socket
		{tcp_closed, _Sock} ->
			put(socket, undefined),
			reflector:send_sync(daemon_client, management, closed, ?SUBS)			
		
	after ?TIMEOUT ->
			
		reflector:sync_to_reflector(?SUBS)
		
	end,
	loop_connection(Port).



close_socket() ->
	Socket=get(socket),
	close_socket(Socket).

close_socket(undefined) ->
	ok;

close_socket(Socket) ->
	gen_tcp:close(Socket),
	put(socket, undefined).



send_to_reflector({MsgType, Msg}) ->
	reflector:send_sync(daemon_server, MsgType, Msg, ?SUBS);

send_to_reflector(Message) ->
	base:elog(?MODULE, "INVALID FORMAT: Message[~p]~n", [Message]).




%% Send a message over the socket connection
%% to the server side. The MsgId field is used
%% to correlate a status message back to the
%% subscriber of this Client interface.
send_to_server(MsgId, Msg) ->
	Socket=get(socket),
	send_to_server(Socket, MsgId, Msg).
		
send_to_server(undefined, MsgId, _Msg) ->
	reflector:send_sync(daemon_client, management, {txerror, MsgId}, ?SUBS);


send_to_server(Socket, MsgId, Msg) ->
	Coded = term_to_binary(Msg),
	case gen_tcp:send(Socket, Coded) of
		ok ->
			reflector:send_sync(daemon_client, management, {txok, MsgId}, ?SUBS);
		{error,Reason} ->
			base:elog(?MODULE, "send_to_server: ERROR, Reason[~p], MsgId[~p] Msg[~p]~n", [Reason, MsgId, Msg]),
			reflector:send_sync(daemon_client, management, {txerror, MsgId}, ?SUBS)
	end.


