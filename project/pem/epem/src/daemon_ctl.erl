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
		 stop_daemon/2
		 ]).

%%
%% LOCAL Exported Functions
%%
-export([
		 get_port/0,
		 create_ctl_file/2,
		 extract_port/1
		 ]).

%%
%% API Functions
%%
start_daemon(Args) ->
	ok.

stop_daemon(_Args) ->
	Port=get_port(),
	daemon_client:send_command(Port, {command, stop}).

stop_daemon(_Args, undefined) ->
	{error, port_undefined};

stop_daemon(_Args, Port) ->
	daemon_client:send_command(Port, {command, stop}).

%% ===============
%% Local Functions
%% ===============

%% Returns the Port# of the
%% daemon currently running (if any)
get_port() ->
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
	try lists:keyfind(port,1,Terms)
	catch
		_ ->
			case lists:keysearch(port,1,Terms) of
				{value, Value} ->
					Value;
				_ ->
					error
			end
	end.


