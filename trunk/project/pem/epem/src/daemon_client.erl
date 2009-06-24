%% Author:      Jean-Lou Dupont
%% Created:     2009-06-23
%% Description: Module for communicating with the daemon
%%
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
		 send_command/2
		 ]).

-export([
		 send_message/2
		 ]).

%%
%% API Functions
%%
start_link(Port) ->
	Pid = spawn(?MODULE, loop_connection, [Port]),
	register(daemon_client, Pid),
	Pid ! {doconnect},
	{ok, Pid}.


send_command(Port, Command) ->
	{Code, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, 2}],?TIMEOUT),
	case Code of
		ok ->
			Ret = send_message(Socket, Command),
			gen_tcp:close(Socket),
			Ret;
				
		_ ->
			{Code, Socket} %%holds the error message
	end.

%%
%% Local Functions
%%
send_message(Socket, Message) ->
	Bin  = term_to_binary(Message),
	gen_tcp:send(Socket, Bin).

