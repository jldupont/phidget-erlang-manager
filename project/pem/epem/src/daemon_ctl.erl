%% Author: Jean-Lou Dupont
%% Created: 2009-06-23
%% Description: TODO: Add description to daemon_ctl
-module(daemon_ctl).

%%
%% API Exported Functions
%%
-export([
		 start_daemon/1,
		 stop_daemon/1,
		 stop_daemon/2,
		 getpid_daemon/0,
		 getpid_daemon/1,
		 send_to_client/1,
		 send_to_client/2,
		 ]).

%%
%% LOCAL Exported Functions
%%
-export([
		 getport/0,
		 saveport/1,
		 extract_port/1
		 ]).

%%
%% API Functions
%%
start_daemon(Args) ->
	%% TODO is there a daemon already running?
	%%      Wait x time for response back
	
	
	%% TODO set_routeto
	ok.



stop_daemon(_Args) ->
	Port=?MODULE:getport(),
	daemon_client:send_command(Port, {command, stop}).

stop_daemon(_Args, undefined) ->
	{error, port_undefined};

stop_daemon(_Args, Port) ->
	daemon_client:send_command(Port, {command, stop}).


%% Asks the daemon side for its Pid.
%% The answer will be relayed to the process configured
%% through the "set_routeto" function.
getpid_daemon() ->
	Port=?MODULE:getport(),
	getpid_daemon(Port).

getpid_daemon({port, Port}) ->
	daemon_client:send_command(Port, {command, pid});

getpid_daemon({Code, Error}) ->
	{Code, Error}.
	

%% ===============
%% Local Functions
%% ===============

%% Returns the Port# of the
%% daemon currently running (if any)
%%
%% Returns: {port, Port}
%%
getport() ->
	{Code, X} = base:read_ctl_file(),
	Terms=X,
	case Code of 
		ok ->
			extract_port(Terms);
		_ ->
			% error code really
			{Code, X}
	end.

extract_port(Terms) ->
	case erlang:is_builtin(lists, keyfind, 3) of
		true  ->
			lists:keyfind(port,1,Terms);
		false ->
			case lists:keysearch(port,1,Terms) of
				{value, Value} ->
					Value;
				_ ->
					error
			end
	end.


%% Save the used by this daemon
saveport(Port) ->
	base:create_ctl_file([{port, Port}]).



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
