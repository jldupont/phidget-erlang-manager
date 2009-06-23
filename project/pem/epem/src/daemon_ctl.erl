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
		 get_port/0
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
	Home = base:home(),
	ok.


