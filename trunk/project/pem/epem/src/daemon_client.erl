%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Module for communicating with the daemon
%%
%% Generates the following message back to the subscribing
%% process (must be set through 'set_routeto'):
%%
%% {Prefix, message, Message}      Message from Server side
%% {Prefix, info, open}            Socket is open & ready
%% {Prefix, info, closed}          Socket is closed
%% {Prefix, info, error}           Socket cannot be opened
%% {Prefix, info, txerror, MsgId}  Error transmitting message to Server
%% {Prefix, info, txok,    MsgId}  Transmission to Server OK

-module(daemon_client).

%%
%% MACROS
%%
-define(TIMEOUT, 2000).

%%
%% Exported Functions
%%
-export([
		 start_link/1,
		 stop/0,
		 set_routeto/2,
		 send_message/2
		 ]).

-export([
		 route/2,
		 route/3,
		 loop_connection/1,
		 close_socket/0,
		 close_socket/1,
		 send_to_server/2,
		 send_to_server/3
		 ]).

%% ======================================================
%% API Functions
%% ======================================================

start_link(Port) ->
	Pid = spawn(?MODULE, loop_connection, [Port]),
	register(daemon_client, Pid),
	Pid ! {doconnect},
	{ok, Pid}.

stop() ->
	daemon_client ! stop.


%% Sets the subscriber parameters.
%% Pid:    process Id of the subscriber wishing to receive
%%         status & messages from the Server
%% Prefix: Term() which should be used to prefix all
%%         status & messages back to the subscriber
set_routeto(Pid, Prefix) when is_pid(Pid) ->
	daemon_client ! {routeto, Pid, Prefix},
	ok;

set_routeto(Proc, Prefix) when is_atom(Proc) ->
	Pid = whereis(Proc),
	daemon_client ! {routeto, Pid, Prefix},
	ok.


%% Sends a message down the socket connection.
%% Status of delivery will be relayed back
%% asynchronously.
send_message(MsgId, Message) ->
	daemon_client ! {send, MsgId, Message},
	ok.


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
					route(info, open);
				_ ->
					put(socket, undefined),
					route(info, error)
			end;

		%% To Server
		{send, MsgId, Msg} ->
			send_to_server(MsgId, Msg);
			
		
		{routeto, Pid, Prefix} ->
			put(routeto, {Pid, Prefix});

		%% From socket
		{tcp, _Sock, Data} ->
			Decoded = binary_to_term(Data),
			route(message, Decoded);

		%% From socket
		{tcp_closed, _Sock} ->
			put(socket, undefined),
			route(info, closed)
		
		
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




%% Route a message back to the subscriber
%% of this Client interface.
%% Use the 'set_routeto' function to setup
%% the communication link.
route(MsgType, Msg) ->
	Routeto=get(routeto),
	route(MsgType, Msg, Routeto).

route(MsgType, Msg, undefined) ->
	%% closing the socket prevents cascading effects
	close_socket(),
	base:elog(?MODULE, "routeto undefined, MsgType[~p] Msg[~p]~n",[MsgType, Msg]);

route(MsgType, Msg, Routeto) ->
	{Pid, Prefix} = Routeto,
	Pid ! {Prefix, MsgType, Msg}.


%% Send a message over the socket connection
%% to the server side. The MsgId field is used
%% to correlate a status message back to the
%% subscriber of this Client interface.
send_to_server(MsgId, Msg) ->
	Socket=get(socket),
	send_to_server(Socket, MsgId, Msg).
		
send_to_server(undefined, MsgId, _Msg) ->
	route(info, txerror, MsgId);

send_to_server(Socket, MsgId, Msg) ->
	Coded = term_to_binary(Msg),
	case gen_tcp:send(Socket, Coded) of
		ok ->
			route(info, txok, MsgId);
		{error,Reason} ->
			base:elog(?MODULE, "send_to_server: ERROR, Reason[~p], MsgId[~p] Msg[~p]~n", [Reason, MsgId, Msg]),
			route(info, txerror, MsgId)
	end.


