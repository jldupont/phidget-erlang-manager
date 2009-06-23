%% Author: Jean-Lou Dupont
%% Created: 2009-06-23
%% Description: TODO: Add description to daemon_client
-module(daemon_client).

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
		 loop_connection/1,
		 send_test_message/1
		 ]).

%%
%% API Functions
%%
start_link(Port) ->
	Pid = spawn(?MODULE, loop_connection, [Port]),
	register(daemon_client, Pid),
	Pid ! {doconnect},
	{ok, Pid}.


%%
%% Local Functions
%%

loop_connection(Port) ->
	receive
		{doconnect} ->
			{Code, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, 2}]),
			case Code of
				ok ->
					send_test_message(Socket),
					gen_tcp:close(Socket);
				
				_ ->
					base:elog("daemon client: error connecting [~p]~n",[Socket])
			end
		
	end, %%RECEIVE
	loop_connection(Port).

send_test_message(Socket) ->
	Term = {test, "Hello!"},
	Bin  = term_to_binary(Term),
	case gen_tcp:send(Socket, Bin) of
		ok ->
			ok;
		Other ->
			base:elog("daemon client: send test message error! [~p]~n", [Other])
	end.

