%% Author: Jean-Lou Dupont
%% Created: 2009-06-19
%% Description: TODO: Add description to ifk
-module(ifk).

%%
%% MACROS
%%
-define(ilog(X,Y), error_logger:info_msg("~p:~p: " X,
                                         [?MODULE, ?LINE | Y])).

-define(DRV_IFK, "/usr/bin/pem_drv_ifk_debug").

%%
%% Exported Functions
%%
-export([
	 start_link/0,
	 stop/0
        ]).

-export([
		 loop/1,
		 loop_handler/1,
		 sync_reflector/1,
		 sync_reflector/2,
		 handle_phidgetdevice/2,
		 filter_device/4,
		 handle_ifk/3,
		 handle_active/2,
		 handle_active/4,
		 handle_inactive/2,
		 handle_inactive/4,
		 init_drv/2,
		 send_to_reflector/1
		 ]).

%%
%% API Functions
%%
start_link() ->
	Pid = spawn(fun() -> loop(unknown) end),
	register( ?MODULE, Pid ),
	error_logger:info_msg("~p:start_link: PID[~p]~n", [?MODULE, Pid]),
	
	% devices handler
	%Pid_handler = spawn(fun() -> loop_handler() end),
	%error_logger:info_msg("~p:start_link: PID_handler[~p]~n", [?MODULE, Pid_handler]),
	
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
		{phidgetdevice, M, Ts} ->
			handle_phidgetdevice(M, Ts);
		
		%%don't know what todo
		Other ->
			error_logger:info_msg("~p: received: [~p]~n", [?MODULE, Other])
	
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
handle_phidgetdevice(Msg, Ts) ->
	%error_logger:info_msg("~p: handle_phidgetdevice, Msg[~p]~n", [?MODULE, Msg]),
	{Serial, Type, State} = Msg,
	filter_device(Serial, Type, State, Ts).

% we just want the InterfaceKit devices!
filter_device(Serial, Type, State, Ts) ->
	case Type of
		"PhidgetInterfaceKit" ->
			handle_ifk(Serial, State,Ts);
	
		_ ->
			notifk
	end.

%% Spawn 1 driver for each InterfaceKit device in "active" state
%%  and get rid of detached device(s)
handle_ifk(Serial, inactive, Ts) ->
	handle_inactive(Serial, Ts),
	ok;

handle_ifk(Serial, active, Ts) ->
	%error_logger:info_msg("~p: handle_ifk: Serial[~p] active~n", [?MODULE, Serial]),
	handle_active(Serial, Ts),
	ok;

handle_ifk(Serial, State, _) ->
	error_logger:error_msg("~p: handle_ifk: Serial[~p] INVALID STATE[~p]~n", [?MODULE, Serial, State]),
	ok.

%% Open the driver if not already done
handle_active(Serial, Ts) ->
	Port = get({port, Serial}),
	Pid  = get({pid,  Serial}),
	handle_active(Serial, Port, Pid, Ts).

handle_active(Serial, _, undefined, Ts) ->
	handle_active(Serial, undefined, invalid, Ts);

% not sure this one is required
handle_active(Serial, undefined, undefined, Ts) ->
	handle_active(Serial, undefined, invalid, Ts);


% Not active... yet
handle_active(Serial, undefined, invalid, _Ts) ->
	Pid = spawn_link(?MODULE, init_drv, [?DRV_IFK, Serial]),
	put({pid, Serial}, Pid),
	error_logger:info_msg("~p: handle_active: Serial[~p] Pid[~p]~n", [?MODULE, Serial, Pid]),	
	ok;

% Is it really active?
handle_active(Serial, _Port, Pid, Ts) ->
	Active = is_process_alive(Pid),
	case Active of
		true ->
			ok;
		false ->
			% clean-up required!
			erase({pid, Serial}),
			erase({port, Serial}),
			handle_active(Serial, undefined, undefined, Ts)
	end.


%% Close the driver if still active
handle_inactive(Serial, Ts) ->
	Port = get({port, Serial}),
	Pid  = get({pid, Serial}),
	handle_inactive(Serial, Port, Pid, Ts),
	ok.

%not even defined it seems... nothing much to do
handle_inactive(_Serial, undefined, _, _) ->
	ok;

handle_inactive(_Serial, _, undefined, _) ->
	ok;

handle_inactive(Serial, Port, Pid, _Ts) ->
	Active = is_process_alive(Pid),
	case Active of
		true ->
			erlang:port_close(Port),
			erlang:exit(Pid, ok),
			erase({port, Serial}),
			erase({pid, Serial}),
			ok;
		false ->
			ok
	end,
	ok.

init_drv(ExtPrg, Serial) ->
	
    process_flag(trap_exit, true),
	Param = erlang:integer_to_list(Serial),
	Port = open_port({spawn, ExtPrg++" "++Param}, [{packet, 2}, binary, exit_status]),
	error_logger:info_msg("~p: init_drv: Serial[~p] Port[~p]~n",[?MODULE, Serial, Port]),
	put({port, Serial}, Port),
	loop_handler(Port).

%% =====================
%% IFK loop
%% =====================

loop_handler(Port) ->
	receive
		{Port, {exit_status, _}} ->
			error_logger:error_msg("~p: loop_handler: an ifk driver exited/could not load~n", [?MODULE]);
			
		{Port, {data, Data}} ->
			Decoded = binary_to_term(Data),
			error_logger:info_msg("~p: loop_handler: decoded msg[~p]~n", [?MODULE, Decoded]),
			send_to_reflector(Decoded);
		
		Msg ->
			error_logger:info_msg("~p: loop_handler: msg[~p]~n", [?MODULE, Msg])
	end,
	loop_handler(Port).


send_to_reflector(Decoded) ->
	{Msgtype, Msg} = Decoded,
	M = {Msgtype, Msg, {date(), time(), now()}},
	try
		reflector ! {self(), M}
	catch
		_:_ -> 
			err
	end.


