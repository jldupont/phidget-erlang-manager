%% Author: Jean-Lou Dupont
%% Created: 2009-06-19
%% Description: TODO: Add description to ifk
-module(ifk).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
	 start_link/0,
	 stop/0
        ]).

-export([
		 loop/1,
		 sync_reflector/1,
		 sync_reflector/2,
		 handle_phidgetdevice/1	 
		 ]).

%%
%% API Functions
%%
start_link() ->
	Pid = spawn(fun() -> loop(unknown) end),
	register( ?MODULE, Pid ),
	error_logger:info_msg("~p:start_link: PID[~p]~n", [?MODULE, Pid]),
	{ok, Pid}.

stop() ->
	?MODULE ! stop.

%%
%% Local Functions
%%
loop(Reflector) ->
	%%error_logger:info_msg("~p: loop: BEGIN", [?MODULE]),
	receive
		stop ->
			error_logger:warning_msg("~p: exiting", [?MODULE]),
			exit(ok);
		{subscribe, ok} ->
			ok;
		{phidgetdevice, M} ->
			handle_phidgetdevice(M);
		Other ->
			error_logger:info_msg("~p: received: [~p]", [?MODULE, Other])
	after 2000 ->
		%%error_logger:info_msg("~p: loop: AFTER", [?MODULE]),
		Updated = sync_reflector(Reflector),
		?MODULE:loop(Updated)
	end,
	?MODULE:loop(Reflector).



sync_reflector(Old) ->
	Current = whereis(reflector),
	sync_reflector(Old, Current).

sync_reflector(Old, Current) when Old == Current ->
	Old;

sync_reflector(Old, Current) when Old /= Current ->
	error_logger:info_msg("~p: sync_reflector: subscribing to 'phidgetdevice'~n", [?MODULE]),
	Response = reflector:subscribe(?MODULE, phidgetdevice),
	error_logger:info_msg("~p: sync_reflector: subscription response[~p]~n", [?MODULE, Response]),
	Current.

%% =====================
%% HANDLER
%% =====================
handle_phidgetdevice(Msg) ->
	error_logger:info_msg("~p: handle_phidgetdevice, Msg[~p]~n", [?MODULE, Msg]),
	ok.
