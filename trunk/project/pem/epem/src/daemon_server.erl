%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Server side of the daemon
%%
%% This module:
%%   - Listens for incoming connections from Clients
%%   - Accepts 1 connection at a time
%%   - Receives messages from a Client
%%   - Send the messages to the Reflector
%%     {from_client, message, Msg}
%%
%%   - Receives messages to be sent to Clients
%%     - Messages are received via the Reflector
%%       {to_client, message, MsgId, Msg}
%%
%%   - Sends messages to clients
%%     - Sends message transmission status through Reflector
%%       {to_client_status, status, Status}
%%       Status = {txerror, MsgId}
%%       Status = {txok,    MsgId}
%%
%%   - Updates the CTL file with the Port currently in use
%%

-module(daemon_server).

-define(REFLECTOR_SYNC_TIMEOUT, 2000).

%% Reflector subscriptions
%%  We just need to grab the messages destined
%%  to the Client side of the connection
-define(SUBS, [to_client]).  

%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 stop/0,
		 send_message/2
		]).

%%
%% Local Functions
%%
-export([
		 start_socket/1,
		 loop_daemon/0,
		 loop_socket/2,
		 route/1,
		 route/2,
		 send_for_client/3,
		 send_to_client/3
		 ]).


%% ======================================================================
%% API Functions
%% ======================================================================
start_link() ->
	Pid = spawn_link(?MODULE, loop_daemon, []),
	register(daemon_server, Pid),
	start_socket(0), %% pick a socket
	{ok, Pid}.

stop() ->
	daemon_server ! stop.

%% Send a message to the Client side
%% of the connection
send_message(MsgId, Msg) ->
	daemon_server ! {to_client, MsgId, Msg}.

%% ======================================================================
%% REFLECTOR INTERFACE
%% ======================================================================

send_to_reflector(Msgtype, Msg) ->
	reflector:publish(M).



%% ======================================================================
%% LOCAL Functions
%% ======================================================================
loop_daemon() ->  %%daemon_server loop
	receive
		
		%% Status of transmission to client side
		{for_client_status, {info, Code, MsgId}} ->
			report_tx_status( {Code, MsgId} );
		
			
		%% The listen port could have been assigned
		%% by the OS: we need to keep track of it
		%% for managing the daemon through a management
		%% client interface. 
		{assignedport, Port} ->
			%% TODO write the port in the CTL file
			put(assignedport, Port);
		
		%% message to send down the socket ... if any
		%% send to socket process for delivery
		{to_client, MsgId, Msg} ->
			SocketPid = get(socket_pid),
			send_for_client(SocketPid, MsgId, Msg);
		
		stop ->
			exit(ok);

		%% Just acting as relay.
		%% This type of message comes from the socket process
		{from_client, Message} ->
			route(Message);
		
		%% Need to track the socket process Pid in order
		%% to establish a communication link between
		%% a Client and a resident daemon
		{daemon_socket_pid, Pid} ->
			put(socket_pid, Pid),
			base:ilog(?MODULE, "socket Pid[~p]~n", [Pid]);
			
		{routeto, Pid, Prefix} ->
			base:ilog(?MODULE, "setting routeto, Pid[~p]~n", [Pid]),			
			put(routeto, {Pid, Prefix});
			
		{closed, Sock} ->
			base:ilog(?MODULE,"Socket[~p] closed~n", [Sock]);
		
		{lsocket, LSocket} ->
			base:ilog(?MODULE,"LSocket[~p]~n", [LSocket]);
		
		{socket, Socket} ->
			base:ilog(?MODULE,"Socket[~p]~n", [Socket]);
		
		{error, lsocket, Reason} ->
			base:elog(?MODULE,"LSocket Error[~p]~n", [Reason]);
		
		{error, socket, Reason} ->
			base:elog(?MODULE,"Socket Error[~p]~n", [Reason])

	after ?REFLECTOR_SYNC_TIMEOUT ->
			
		reflector:sync_to_reflector(?SUBS)
	
	end,
	loop_daemon().




%% CALLED FROM daemon_server PROCESS	
%% Send status/message back to the subscriber
route(Message) ->
	Routeto=get(routeto),
	route(Message, Routeto).

route(_Message, undefined) ->
	%% TODO probably need to throttle this!
	base:ilog(?MODULE, "route: routeto undefined~n", []);

route(Message, Routeto) ->
	{Pid, Prefix} = Routeto,
	Pid ! {Prefix, Message}.





%% CALLED FROM daemon_server PROCESS
%% These functions server as bridge between the daemon_server
%% process and the socket process
send_for_client(undefined, MsgId, Msg) ->
	%% TODO throttle this?
	base:elog(?MODULE, "socket process ERROR, cannot send MsgId[~p] Msg[~p]~n", [MsgId, Msg]);

send_for_client(SocketPid, MsgId, Msg) ->
	SocketPid ! {for_client, MsgId, Msg}.




start_socket(Port) ->
	{Code, LSocket} = gen_tcp:listen(Port,[{nodelay, true}, {packet, 2}, {reuseaddr, true},{active, true}]),
	case Code of
		ok ->
			%% Retrieve the Port number as it could have been assigned by the OS
			%% by setting '0' in the call to gen_tcp:listen
			AssignedPort = inet:port(LSocket),
			daemon_server ! {lsocket, LSocket},

			%%TODO check this: process_flag(trap_exit, true),
			Pid = spawn_link(?MODULE, loop_socket, [AssignedPort, LSocket]),
			register(daemon_socket, Pid),
			
			%% info back to the main loop 'daemon_server'
			daemon_server ! {assignedport, AssignedPort},
			daemon_server ! {daemon_socket_pid, Pid},
			
			%% Start accepting calls!
			daemon_socket ! {dostart};
		_ ->
			daemon_server ! {error, lsocket, LSocket}
	end,
	ok.



%% =========================================================================================================================
%% =========================================================================================================================
%% Socket process loop
%% =========================================================================================================================
%% =========================================================================================================================



loop_socket(Port, LSocket) ->
	receive
		%% waits for a connection i.e. BLOCKING <=======================================
		%% =============================================================================
		{dostart} ->
			{Code, Socket} = gen_tcp:accept(LSocket),
			inet:setopts(Socket, [{packet,2},binary,{nodelay, true},{active, true}]),
			case Code of
				ok ->
					put(socket, Socket),
					daemon_server ! {socket, Socket};
				_ ->
					daemon_server ! {error, socket, Socket},
					 %% try again
					 %% TODO think about limiting this?
					self() ! {dostart}
			end;

		%% Message Reception
		%% Rx from Client connected on the socket,
		%% message is relayed through the daemon_server
		%% to the subscriber to all messages.
		%% The subscriber is configured through "set_routeto" function.
		{tcp, Sock, Data} ->
			Decoded = binary_to_term(Data),
			daemon_server ! {from_client, {message, Decoded}},
			base:ilog(?MODULE, "tcp socket: Sock[~p] Decoded[~p]~n", [Sock, Decoded]);
		
		%% Client Connection close... restart
		{tcp_closed, Sock} ->
			daemon_server ! {closed, Sock},
			self() ! {dostart};
		
		%% Message to deliver to the Client connected to the socket
		{for_client, MsgId, Msg} ->
			Socket=get(socket),
			send_to_client(Socket, MsgId, Msg);
		
		Other ->
			base:elog(?MODULE, "socket: Other[~p]~n", [Other])
		

	end, %%RECEIVE
	loop_socket(Port, LSocket).



%% CALLED BY SOCKET PROCESS
%% Send message down the socket to the Client side
send_to_client(undefined, MsgId, Msg) ->
	%% TODO throttle?
	base:elog(?MODULE, "socket error, cannot send, MsgId[~p] Msg[~p]~n", [MsgId, Msg]),
	daemon_server ! {for_client_status, {info, txerror, MsgId}};


send_to_client(Socket, MsgId, Msg) ->
	base:elog(?MODULE, "Message to client, MsgId[~p] Msg[~p]~n", [MsgId, Msg]),
	Coded = term_to_binary(Msg),
	case gen_tcp:send(Socket, Coded) of
		ok ->
			daemon_server ! {for_client_status, {info, txok, MsgId}};

		{error, Reason} ->
			base:elog(?MODULE, "send_to_client: ERROR, Reason[~p] MsgId[~p] Msg[~p]~n", [Reason, MsgId, Msg]),
			daemon_server ! {for_client_status, {info, txerror, MsgId}}
	end.


