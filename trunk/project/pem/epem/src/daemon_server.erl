%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Server side of the daemon
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
		 route/2
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
		stop ->
			exit(ok);

		{message, Message} ->
			route(Message);
		
		{daemon_socket_pid, Pid} ->
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
					daemon_server ! {socket, Socket};
				_ ->
					daemon_server ! {error, socket, Socket},
					 %% try again
					 %% TODO think about limiting this?
					self() ! {dostart}
			end;

		%% Message Reception
		{tcp, Sock, Data} ->
			Decoded = binary_to_term(Data),
			daemon_server ! {message, Decoded},
			error_logger:info_msg("daemon server: tcp socket: Sock[~p] Decoded[~p]~n", [Sock, Decoded]);
		
		%% Client Connection close... restart
		{tcp_closed, Sock} ->
			daemon_server ! {closed, Sock},
			self() ! {dostart};
		
		Other ->
			error_logger:info_msg("daemon server: socket: Other[~p]~n", [Other])
		
	end, %%RECEIVE
	loop_socket(Port, LSocket).


route(Message) ->
	Routeto=get(routeto),
	route(Message, Routeto).

route(_Message, undefined) ->
	base:ilog(?MODULE, "route: routeto undefined~n", []);

route(Message, Routeto) ->
	Routeto ! {daemon_message, Message}.



