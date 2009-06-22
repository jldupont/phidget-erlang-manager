%% Author: Jean-Lou Dupont
%% Created: 2009-06-19
%% Description: TODO: Add description to ifk
-module(ifk).

%%
%% MACROS
%%
-define(ilog(X,Y), error_logger:info_msg("~p:~p: " X,
                                         [?MODULE, ?LINE | Y])).

-define(DRV_IFK_DEBUG, "pem_drv_ifk_debug").
-define(DRV_IFK,       "pem_drv_ifk").

%%
%% Exported Functions
%%
-export([
	 start_link/0,
	 start_link/1,	 
	 stop/0
        ]).

-export([
		 start/1,
		 loop/0,
		 loop_handler/1,
		 sync_reflector/0,
		 sync_reflector/2,
		 handle_phidgetdevice/2,
		 filter_device/4,
		 handle_ifk/3,
		 handle_active/2,
		 handle_active/4,
		 handle_inactive/2,
		 handle_inactive/4,
		 ifk_drv/2,
		 send_to_reflector/1,
		 handle_crashed_driver/1,
		 clean_driver/1
		 ]).

%% =============
%% API Functions
%% =============
start_link(Args) ->
	Debug = Args--["debug"],
	case Debug of
		"debug" ->
			Driver = ?DRV_IFK_DEBUG;
		_Other ->
			Driver = ?DRV_IFK
	end,
	start(Driver).


start_link() ->
	start(?DRV_IFK).
	
start(DriverPath) ->
	Pid = spawn(?MODULE, loop, []),
	register( ?MODULE, Pid ),
	?MODULE ! {driver_path, DriverPath},
	{ok, Pid}.

stop() ->
	?MODULE ! stop.

%% =====================================================
%% MAIN LOOP
%% =====================================================
loop() ->
	receive
		{driver_path, DriverPath} ->
			put(driver_path, DriverPath);
		
		stop ->
			error_logger:warning_msg("~p: exiting", [?MODULE]),
			exit(ok);

		{reflector, subscribe, ok} ->
			ok;
		
		%%verify that it is an "InterfaceKit" device
		{phidgetdevice, M, Ts} ->
			error_logger:info_msg("~p: loop: received 'phidgetdevice'~n", [?MODULE]),
			handle_phidgetdevice(M, Ts);
		
		{driver, Serial, Port, Pid} ->
			error_logger:info_msg("~p: loop: received driver info, Serial[~p] Port[~p] Pid[~p]~n", [?MODULE, Serial, Port, Pid]),
			put({port,  Serial}, Port),
			put({pid,   Serial}, Pid),
			put({serial, Port},  Serial);
		
		{crashed, Port} ->
			clean_driver(Port);
		
		%%don't know what todo
		Other ->
			error_logger:info_msg("~p: received: [~p]~n", [?MODULE, Other])
	
	after 2000 ->

		sync_reflector()
	
	end,
	?MODULE:loop().

clean_driver(undefined) ->
	error_logger:warning_msg("~p: clean_driver: received undefined", [?MODULE]);

clean_driver(Port) ->
	Serial = get({serial, Port}),
	error_logger:info_msg("~p: clean_driver: Serial[~p] Port[~p]~n", [?MODULE, Serial, Port]),
	erase({port, Serial}),
	erase({pid, Serial}),
	erase({serial, Port}).


%% =====================
%% Sync to Reflector
%% =====================

sync_reflector() ->
	Current = whereis(reflector),
	Old = get(reflector_pid),
	sync_reflector(Old, Current).

sync_reflector(Old, Current) when Old == Current ->
	%%error_logger:info_msg("~p: sync_reflector: unchanged~n", [?MODULE]),
	Old;

sync_reflector(Old, Current) when Old /= Current ->
	Response = reflector:subscribe(?MODULE, phidgetdevice),
	error_logger:info_msg("~p: sync_reflector: subscription response[~p]~n", [?MODULE, Response]),
	put(reflector_pid, Current),
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
	DriverPath = get(driver_path),
	%%error_logger:info_msg("~p: handle_active: DriverPath[~p]~n", [?MODULE, DriverPath]),
	Pid = spawn(?MODULE, ifk_drv, [DriverPath, Serial]),
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
			erlang:exit(Pid, kill),
			erase({port, Serial}),
			erase({pid,  Serial}),
			ok;
		false ->
			ok
	end,
	ok.

ifk_drv(ExtPrg, Serial) ->
	%%error_logger:info_msg("~p:ifk_drv: Serial[~p] Pid[~p]~n",[?MODULE, Serial, self()]),
    process_flag(trap_exit, true),
	Param = erlang:integer_to_list(Serial),
	Port = open_port({spawn, ExtPrg++" "++Param}, [{packet, 2}, binary, exit_status]),
	error_logger:info_msg("~p: ifk_drv: Serial[~p] Port[~p] Pid[~p]~n",[?MODULE, Serial, Port, self()]),
	put({port,   Serial}, Port),
	put({serial, Port},   Serial),
	
	% signal back some useful mapping
	?MODULE ! {driver, Serial, Port, self()},
	loop_handler(Port).


%% =====================
%% IFK DRIVER loop
%% =====================


loop_handler(Port) ->
	receive
			
		{Port, {exit_status, _}} ->
			error_logger:error_msg("~p: loop_handler: an ifk driver exited/could not load~n", [?MODULE]),
			handle_crashed_driver(Port),
			exit(crashed);
			
			
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



%% =======================
%% CRASHED DRIVER RECOVERY
%% =======================


handle_crashed_driver(Port) ->
	error_logger:warning_msg("~p: handle_crashed_driver: Port[~p] Pid[~p]~n", [?MODULE, Port, self()]),
	?MODULE ! {crashed, Port}.

