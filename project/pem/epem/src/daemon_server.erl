%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Server side of the daemon
%%
%% -- Verify daemon is running
%%    -- Check CTL file
%%    -- Get port from CTL
%%    -- Try issuing a command
%%
-module(daemon_server).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 start_link/1
		]).


-export([
		 start_socket/1,
		 loop_daemon/0,
		 loop_socket/2
		 ]).
%%
%% API Functions
%%
start_link(Port) ->
	Pid = spawn(?MODULE, loop_daemon, []),
	register(daemon_server, Pid),
	start_socket(Port),
	{ok, Pid}.


%%
%% Local Functions
%%
loop_daemon() ->
	receive
		{closed, Sock} ->
			base:ilog("daemon server: Socket[~p] closed~n", [Sock]);
		
		{lsocket, LSocket} ->
			base:ilog("daemon server: LSocket[~p]~n", [LSocket]);
		
		{socket, Socket} ->
			base:ilog("daemon server: Socket[~p]~n", [Socket]);
		
		{error, lsocket, Reason} ->
			base:elog("daemon server: LSocket Error[~p]~n", [Reason]);
		
		{error, socket, Reason} ->
			base:elog("daemon server: Socket Error[~p]~n", [Reason])

	end,
	loop_daemon().


start_socket(Port) ->
	{Code, LSocket} = gen_tcp:listen(Port,[{nodelay, true}, {packet, 2}, {reuseaddr, true},{active, true}]),
	case Code of
		ok ->
			daemon_server ! {lsocket, LSocket},
			process_flag(trap_exit, true),
			Pid = spawn(?MODULE, loop_socket, [Port, LSocket]),
			register(daemon_socket, Pid),
			daemon_socket ! {dostart};
		_ ->
			daemon_server ! {error, lsocket, LSocket}
	end,
	ok.



loop_socket(Port, LSocket) ->
	receive
		%% waits for a connection
		{'EXIT', _Reason} ->
			self() ! dostart;
		
		{dostart} ->
			{Code, Socket} = gen_tcp:accept(LSocket),
			inet:setopts(Socket, [{packet,2},binary,{nodelay, true},{active, true}]),
			case Code of
				ok ->
					daemon_server ! {socket, Socket};
				_ ->
					daemon_server ! {error, socket, Socket}
			end;

		{tcp, Sock, Data} ->
			Decoded = binary_to_term(Data),
			error_logger:info_msg("daemon server: tcp socket: Sock[~p] Decoded[~p]~n", [Sock, Decoded]);
		
		{Port, {data, Data}} ->
			Decoded = binary_to_term(Data),
			error_logger:info_msg("daemon server: socket: Decoded[~p]~n", [Decoded]);

		{tcp_closed, Sock} ->
			daemon_server ! {closed, Sock},
			self() ! {dostart};
		
		Other ->
			error_logger:info_msg("daemon server: socket: Other[~p]~n", [Other])
		
	end, %%RECEIVE
	loop_socket(Port, LSocket).



