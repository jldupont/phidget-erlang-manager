%% Author: Jean-Lou Dupont
%% Created: 2009-06-22
%% Description: TODO: Add description to pem
%%
%% start()            -> starts daemon
%% start(debug)       -> starts daemon
%% start(debug,start) -> starts daemon
%% start(debug,stop)  -> stops daemon
%% start(stop)        -> stops daemon
%%
-module(pem_app).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 stop/0,
	 start/0,
	 start/1,
	 start/2,
	 loop/0,
	 process_management_message/1,
	 send_to_client/1,
	 send_to_client/2
        ]).

-export([
		 start_daemon/1,
		 stop_daemon/1
		 ]).

%% ====================================================================!
%% API functions
%% ====================================================================!
start() ->
	start_daemon([]).

start(debug) ->
	start_daemon([debug]);

start(stop) ->
	stop_daemon([]).

start(debug, start) ->
	start_daemon([debug]);

start(debug, stop) ->
	stop_daemon([debug]).

start_daemon(Args) ->
	base:ilog(?MODULE, "Args[~p]~n", [Args]),
	process_flag(trap_exit,true),
	pem_sup:start_link(Args),
	loop().

stop_daemon(_Args) ->
	ok.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
    ?MODULE ! stop.


%% ====================================================================!
%% LOCAL functions
%% ====================================================================!

loop() ->
	receive
		{daemon_management_message, Msg} ->
			process_management_message(Msg);
		
		stop ->
			exit(ok);
		Other->
			base:elog(?MODULE, "received unknown message [~p]~n", [Other])

	end,
	loop().


%% Management Client issued message
process_management_message(Msg) ->
	case Msg of
		stop ->
			?MODULE ! stop;
		
		pid ->
			Pid = os:getpid(),
			send_to_client({daemon_message, pid, Pid})
	end.



%% Send a message back to the management client
%% (assuming one is connected)
%% The message is relayed through the module "daemon_server"
%%
send_to_client(Msg) ->
	Server = whereis(daemon_server),
	send_to_client(Msg, Server).

send_to_client(Msg, undefined) ->
	base:elog(?MODULE, "daemon_server: NOT FOUND, unable to send[~p]~n",[Msg]);

send_to_client(Msg, Server) ->
	Server ! Msg.
