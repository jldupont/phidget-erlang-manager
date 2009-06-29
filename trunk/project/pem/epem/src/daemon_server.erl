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
%%
%%       {to_client, {MsgId, Msg} }
%%        ^          ------------
%%        |                ^
%%        MsgType          |
%%                        Msg
%%
%%   - Receives messages from the socket
%%     These are sent to the Reflector with the following:
%%
%%       {from_client, Msg}
%%
%%   - Sends messages to clients
%%     - Sends message transmission status through Reflector
%%
%%       {to_client_tx_status, {MsgId, Code} }
%%       Status Code = txerror || txok 
%%
%%   - Sends the Port opened for management Clients on the Reflector
%%
%%     {management_port, Port}
%%           ^            
%%           |
%%        MsgType

-module(daemon_server).

-define(ASSIGNEDPORT_TIMEOUT, 2000).

%% Reflector subscriptions
%%  We just need to grab the messages destined
%%  to the Client side of the connection
-define(SUBS, [to_client]).  

%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 start_link/1,
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
		 send_for_client/3,
		 send_to_client/3
		 ]).


%% ======================================================================
%% API Functions
%% ======================================================================
start_link() ->
	start_link([]).

start_link(Args) ->
	Pid = spawn_link(?MODULE, loop_daemon, []),
	register(daemon_server, Pid),
	?MODULE ! {args, Args},
	start_socket(0), %% pick a socket
	{ok, Pid}.

stop() ->
	daemon_server ! stop.

%% Send a message to the Client side
%% of the connection
send_message(MsgId, Msg) ->
	daemon_server ! {to_client, MsgId, Msg}.



%% ======================================================================
%% LOCAL Functions
%% ======================================================================
loop_daemon() ->  %%daemon_server loop
	receive
		{args, Args} ->
			put(args, Args),
			switch:subscribe(daemon_server, ?SUBS);
		
		{switch, subscribed} ->
			%%base:ilog(?MODULE, "subscribed~n",[]),
			switch:publish(daemon_server, ready, self());
		
		stop ->
			exit(ok);
		
		%% Status of transmission to client side
		{tx_status, {info, Code, MsgId}} ->
			switch:publish(?MODULE, to_client_tx_status, {MsgId, Code});

		
		%% The listen port could have been assigned
		%% by the OS: we need to keep track of it
		%% for managing the daemon through a management
		%% client interface. 
		
		{assignedport, Port} ->
			put(assignedport, Port),
			switch:publish(?MODULE, management_port, Port);

		
		%% message to send down the socket ... if any.
		%% send to socket process for delivery
		{to_client, {MsgId, Msg}} ->
			SocketPid = get(socket_pid),
			send_for_client(SocketPid, MsgId, Msg);
		
		%% Just acting as relay.
		%% This type of message comes from the socket process
		%% (from the Client side) and needs to be relayed.
		%% The message is sent on the Reflector.
		{from_client, Message} ->
			switch:publish(?MODULE, from_client, Message);


		
		%% Need to track the socket process Pid in order
		%% to establish a communication link between
		%% a Client and a resident daemon
		{daemon_socket_pid, Pid} ->
			put(socket_pid, Pid),
			base:ilog(?MODULE, "socket Pid[~p]~n", [Pid]);
			
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

	%% We always have to sync to the reflector;
	%% we can't rely on the having to send stuff
	%% to re-sync.
	after ?ASSIGNEDPORT_TIMEOUT ->

		Port=get(assignedport),
		switch:publish(?MODULE, management_port, Port)
	
	end,
	loop_daemon().





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

			Pid = spawn_link(?MODULE, loop_socket, [AssignedPort, LSocket]),
			register(daemon_socket, Pid),
			
			%% info back to the main loop 'daemon_server'
			{_, Porte} = AssignedPort,
			base:ilog(?MODULE, "assignedport [~p]~n",[Porte]),
			
			daemon_server ! {assignedport, Porte},
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
		%% message is relayed through the daemon_server.
		{tcp, Sock, Data} ->
			Message = binary_to_term(Data),
			daemon_server ! {from_client, Message},
			base:ilog(?MODULE, "tcp socket: Sock[~p] Decoded[~p]~n", [Sock, Message]);
		
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
	daemon_server ! {tx_status, {info, txerror, MsgId}};


send_to_client(Socket, MsgId, Msg) ->
	base:elog(?MODULE, "Message to client, MsgId[~p] Msg[~p]~n", [MsgId, Msg]),
	Coded = term_to_binary(Msg),
	case gen_tcp:send(Socket, Coded) of
		ok ->
			daemon_server ! {tx_status, {info, txok, MsgId}};

		{error, Reason} ->
			base:elog(?MODULE, "send_to_client: ERROR, Reason[~p] MsgId[~p] Msg[~p]~n", [Reason, MsgId, Msg]),
			daemon_server ! {tx_status, {info, txerror, MsgId}}
	end.


