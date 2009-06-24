%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Server side of the daemon
%%
%% To send a message back to the management client,
%% send a message to this module using {daemon_message, Msg}.
%%
-module(daemon_server).

%%
%% Exported Functions
%%
-export([
		 start_link/1,
		 set_routeto/1,
		 stop/0
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
		 send_for_client/2,
		 send_to_client/2
		 ]).

%% ======================================================================
%% API Functions
%% ======================================================================
start_link(Port) ->
	Pid = spawn(?MODULE, loop_daemon, []),
	register(daemon_server, Pid),
	start_socket(Port),
	{ok, Pid}.

stop() ->
	daemon_server ! stop.



%% Routes all the valid messages to Pid
set_routeto(Pid) when is_pid(Pid) ->
	daemon_server ! {routeto, Pid},
	{ok, pid, Pid};

set_routeto(Proc) when is_atom(Proc) ->
	Pid = whereis(Proc),
	daemon_server ! {routeto, Pid},
	{ok, atom, Pid}.




%% ======================================================================
%% LOCAL Functions
%% ======================================================================
loop_daemon() ->
	receive
		%% message to send to the management client... if any
		%% send to socket process for delivery
		{daemon_message, Msg} ->
			SocketPid = get(socket_pid),
			send_for_client(SocketPid, Msg);
		
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
			
		{routeto, Pid} ->
			base:ilog(?MODULE, "setting routeto, Pid[~p]~n", [Pid]),			
			put(routeto, Pid);
			
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

	end,
	loop_daemon().



send_for_client(undefined, Msg) ->
	base:elog(?MODULE, "socket process ERROR, cannot send [~p]~n", [Msg]);

send_for_client(SocketPid, Msg) ->
	SocketPid ! {for_client, Msg}.



start_socket(Port) ->
	{Code, LSocket} = gen_tcp:listen(Port,[{nodelay, true}, {packet, 2}, {reuseaddr, true},{active, true}]),
	case Code of
		ok ->
			daemon_server ! {lsocket, LSocket},
			process_flag(trap_exit, true),
			Pid = spawn_link(?MODULE, loop_socket, [Port, LSocket]),
			register(daemon_socket, Pid),
			daemon_server ! {daemon_socket_pid, Pid},
			daemon_socket ! {dostart};
		_ ->
			daemon_server ! {error, lsocket, LSocket}
	end,
	ok.



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
			daemon_server ! {from_client, Decoded},
			error_logger:info_msg("daemon server: tcp socket: Sock[~p] Decoded[~p]~n", [Sock, Decoded]);
		
		%% Client Connection close... restart
		{tcp_closed, Sock} ->
			daemon_server ! {closed, Sock},
			self() ! {dostart};
		
		%% Message to deliver to the Client connected to the socket
		{for_client, Msg} ->
			Socket=get(socket),
			send_to_client(Socket, Msg);
		
		Other ->
			error_logger:info_msg("daemon server: socket: Other[~p]~n", [Other])
		
	end, %%RECEIVE
	loop_socket(Port, LSocket).




send_to_client(undefined, Msg) ->
	base:elog(?MODULE, "socket error, cannot send[~p]~n", [Msg]);

send_to_client(Socket, Msg) ->
	base:elog(?MODULE, "Message to client[~p]~n", [Msg]),
	Coded = term_to_binary(Msg),
	gen_tcp:send(Socket, Coded).


	

route(Message) ->
	Routeto=get(routeto),
	route(Message, Routeto).

route(_Message, undefined) ->
	base:ilog(?MODULE, "route: routeto undefined~n", []);

route(Message, Routeto) ->
	Routeto ! {daemon_management_message, Message}.



