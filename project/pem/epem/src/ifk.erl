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
		 loop_handler/0,
		 sync_reflector/1,
		 sync_reflector/2,
		 handle_phidgetdevice/1,
		 filter_device/3,
		 handle_ifk/2,
		 handle_active/1,
		 handle_active/2,
		 handle_inactive/1,
		 handle_inactive/2
		 ]).

%%
%% API Functions
%%
start_link() ->
	Pid = spawn(fun() -> loop(unknown) end),
	register( ?MODULE, Pid ),
	error_logger:info_msg("~p:start_link: PID[~p]~n", [?MODULE, Pid]),
	
	% devices handler
	Pid_handler = spawn(fun() -> loop_handler() end),
	error_logger:info_msg("~p:start_link: PID_handler[~p]~n", [?MODULE, Pid_handler]),
	
	{ok, Pid}.

stop() ->
	?MODULE ! stop.

%% =====================================================
%% MAIN LOOP
%% =====================================================
loop(Reflector) ->
	receive
		stop ->
			error_logger:warning_msg("~p: exiting", [?MODULE]),
			exit(ok);

		{reflector, subscribe, ok} ->
			ok;
		
		%%verify that it is an "InterfaceKit" device
		{phidgetdevice, M} ->
			handle_phidgetdevice(M);
		
		%%don't know what todo
		Other ->
			error_logger:info_msg("~p: received: [~p]", [?MODULE, Other])
	
	after 2000 ->

		Updated = sync_reflector(Reflector),
		?MODULE:loop(Updated)
	
	end,
	?MODULE:loop(Reflector).


%% =====================
%% Sync to Reflector
%% =====================

sync_reflector(Old) ->
	Current = whereis(reflector),
	sync_reflector(Old, Current).

sync_reflector(Old, Current) when Old == Current ->
	Old;

sync_reflector(Old, Current) when Old /= Current ->
	_Response = reflector:subscribe(?MODULE, phidgetdevice),
	%%error_logger:info_msg("~p: sync_reflector: subscription response[~p]~n", [?MODULE, Response]),
	Current.

%% =====================
%% HANDLER
%% =====================
handle_phidgetdevice(Msg) ->
	%error_logger:info_msg("~p: handle_phidgetdevice, Msg[~p]~n", [?MODULE, Msg]),
	{Serial, Type, State} = Msg,
	filter_device(Serial, Type, State).

% we just want the InterfaceKit devices!
filter_device(Serial, Type, State) ->
	case Type of
		"PhidgetInterfaceKit" ->
			handle_ifk(Serial, State);
	
		_ ->
			notifk
	end.

%% Spawn 1 driver for each InterfaceKit device in "active" state
%%  and get rid of detached device(s)
handle_ifk(Serial, "inactive") ->
	ok;

handle_ifk(Serial, "active") ->
	error_logger:info_msg("~p: handle_ifk: Serial[~p] active~n", [?MODULE, Serial]),
	ok;

handle_ifk(Serial, State) ->
	error_logger:error_msg("~p: handle_ifk: Serial[~p] INVALID STATE[~p]~n", [?MODULE, Serial, State]),
	ok.

%% Open the driver if not already done
handle_active(Serial) ->
	Drv_port = get({port, Serial}),
	handle_active(Serial, Drv_port).

% nothing to do it seems
handle_active(_Serial, undefined) ->
	ok;

% Is it really active?
handle_active(Serial, Port) ->
	ok.

%% Close the driver if still active
handle_inactive(Serial) ->
	ok.

handle_inactive(_Serial, undefined) ->
	ok;

handle_inactive(Serial, Port) ->
	ok.



%% =====================
%% IFK loop
%% =====================

loop_handler() ->
	ok.
