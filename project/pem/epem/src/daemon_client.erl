%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Module for communicating with the daemon
%%
%% SUBSCRIPTIONS:
%% ==============
%%
%% {to_daemon,       {MsgId, Msg}}
%% {management_port, Port}
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

-module(daemon_client).

%%
%% MACROS
%%
-define(TIMEOUT, 2000).

-define(SUBS, [to_daemon, management_port, client]).

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
		 send_to_reflector/1,
		 send_to_server/2,
		 send_to_server/3
		 ]).

%% ======================================================
%% API Functions
%% ======================================================

start_link()->
	Pid = spawn_link(?MODULE, loop_connection, []),
	register(daemon_client, Pid),
	?MODULE ! {sync,undefined, undefined},
	%%base:ilog(?MODULE,"Pid[~p]~n",[Pid]),
	{ok, Pid}.

%% Once started, sends Msg to Recipient
start_link({Recipient, Msg}) ->
	Pid = spawn_link(?MODULE, loop_connection, []),
	register(daemon_client, Pid),
	?MODULE ! {sync,Recipient, Msg},
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
		{sync, Recipient, Msg} ->
			put(root_proc, Recipient),
			base:send_ready_signal(daemon_client, Recipient, Msg);
			

		%% All modules are ready... let's sync
		%% to the Reflector
		mods_ready ->
			reflector:sync_to_reflector(?SUBS),
			ok;
		
		%% We are subscribed... final sync to the root proc
		{from_reflector, subscribed} ->
			RootProc=get(root_proc),
			%%base:send_on_count(RootProc, Msg, CountVar, TargetCount)
			base:send_synced_signal(daemon_client, RootProc),
			ok;
		
		{management_port, Port} ->
			put(management_port, Port);
		
		stop ->
			close_socket(),
			exit(ok);
		
		%% Starts a connection towards the Server
		{client, doconnect} ->
			%%io:format("doconnect~n"),
			close_socket(),
			Port=get(management_port),
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
			%%io:format("received: ~p~n",[Message]),
			send_to_reflector(Message);

		%% From socket
		{tcp_closed, _Sock} ->
			put(socket, undefined),
			reflector:send_sync(daemon_client, management, closed, ?SUBS)			
		
	%%after ?TIMEOUT ->
			
		%%reflector:sync_to_reflector(?SUBS)
		
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



send_to_reflector({MsgType, Msg}) ->
	reflector:send_sync(daemon_server, from_daemon, {MsgType, Msg}, ?SUBS);

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


