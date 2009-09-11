%% Author: Jean-Lou Dupont
%% Created: 2009-06-19
%% Description: Phidget InterfaceKit driver
%%
%% MESSAGES GENERATED:
%% ===================
%% {phidgeterror, {{Serial, Code, String},                  date(), time(), now() }}
%% {device,       {{Serial,Type,State, Version,Name,Label}, date(), time(), now() }}
%% {din,          {{Serial, Index, Value},                  date(), time(), now() }}
%% {dout,         {{Serial, Index, Value},                  date(), time(), now() }}
%% {sout,         {{Serial, Index, Value},                  date(), time(), now() }}
%%
%%
%% SUBSCRIPTIONS:
%% ==============
-module(epem_ifk).

-define(SERVER_DRV, ifk_driver).
-define(SERVER,     ifk).
-define(BUSSES,     [log, sys, clock, phidgets]).
-define(CTOOLS,     mswitch_ctools).
-define(SWITCH,     epem_hwswitch).


%%
%% MACROS
%%
-define(DRV_IFK_DEBUG, "pem_drv_ifk_debug").
-define(DRV_IFK,       "pem_drv_ifk").

-define(SUBS,          [phidgetdevice]).

%%
%% Exported Functions
%%
-export([
	 start_link/0,		 
	 stop/0
        ]).

-export([
		 loop/0,
		 loop_handler/1,
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
start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register( ?SERVER, Pid ),
	{ok, Pid}.

stop() ->
	try ?SERVER ! stop
	catch _:_ -> noop
	end.


%% =====================================================
%% MAIN LOOP
%% =====================================================
loop() ->
	receive
		
		stop ->
			exit(normal);

		%%%% CONFIGURATION MANAGEMENT
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		%%% LOCAL SWITCH RELATED %%%
		%%% --------------------
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});

		
		
		%%verify that it is an "InterfaceKit" device
		{_From, phidgetdevice, {M, Ts}} ->
			%%base:ilog(?MODULE,"received 'phidgetdevice'~n", []),
			handle_phidgetdevice(M, Ts);

		%% Message from the ifk_driver
		%%
		{driver, Serial, Port, Pid} ->
			log(debug, "driver info {Serial, Port, Pid}: ", [[Serial, Port, Pid]]),
			put({port,  Serial}, Port),
			put({pid,   Serial}, Pid),
			put({serial, Port},  Serial);
		
		%% From ifk_driver
		%%
		{crashed, Port} ->
			log(warning, "ifk: driver crashed on port: ", [Port]),
			clean_driver(Port);
		
		%%don't know what todo
		Other ->
			log(debug,"ifk: unknown msg: ", [Other])
	
	end,
	loop().

clean_driver(undefined) ->
	noop;

clean_driver(Port) ->
	Serial = get({serial, Port}),
	erase({port, Serial}),
	erase({pid, Serial}),
	erase({serial, Port}).


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HWSWITCH  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

handle({hwswitch, _From, clock, {tick.min, Count}}) ->
	put(clock.min, Count),
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;

handle({hwswitch, _From, sys, app.ready}) ->
	todo;


handle({hwswitch, _From, sys, suspend}) -> noop;
handle({hwswitch, _From, sys, resume}) -> noop;

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	put(config.version.inforce, VersionInForce);


%% A module advertises its configuration version
handle({hwswitch, _From, sys, {mod.config, Module, Version}}) ->
	put({mod.version, Module}, Version);


handle({hwswitch, _From, sys, _Msg}) ->
	not_supported;


handle({hwswitch, _From, phidgets, _Msg}) ->
	todo;
	

handle(Other) ->
	log(warning, "ifk: Unexpected message: ", [Other]).




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
	handle_inactive(Serial, Ts);

handle_ifk(Serial, active, Ts) ->
	%error_logger:info_msg("~p: handle_ifk: Serial[~p] active~n", [?MODULE, Serial]),
	handle_active(Serial, Ts);

handle_ifk(Serial, State, _) ->
	log(debug, "ifk: invalid state {Serial, State}: ", [[Serial, State]]);


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
	Pid = spawn(?MODULE, ifk_drv, [DriverPath, Serial]),
	log(debug, "ifk: handle active {Serial, Pid}: ", [[Serial, Pid]]);


% Is it really active?
handle_active(Serial, _Port, Pid, Ts) ->
	Active = is_process_alive(Pid),
	case Active of
		true  -> ok;
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
	handle_inactive(Serial, Port, Pid, Ts);

%not even defined it seems... nothing much to do
handle_inactive(_Serial, undefined, _, _) -> ok;
handle_inactive(_Serial, _, undefined, _) -> ok;

handle_inactive(Serial, Port, Pid, _Ts) ->
	Active = is_process_alive(Pid),
	case Active of
		true ->
			erlang:port_close(Port),
			erlang:exit(Pid, kill),
			erase({port, Serial}),
			erase({pid,  Serial}),
			ok;
		false -> ok
	end.

ifk_drv(ExtPrg, Serial) ->
	%%error_logger:info_msg("~p:ifk_drv: Serial[~p] Pid[~p]~n",[?MODULE, Serial, self()]),
    process_flag(trap_exit, true),
	Param = erlang:integer_to_list(Serial),
	Port = open_port({spawn, ExtPrg++" "++Param}, [{packet, 2}, binary, exit_status]),
	error_logger:info_msg("~p: ifk_drv: Serial[~p] Port[~p] Pid[~p]~n",[?MODULE, Serial, Port, self()]),
	put({port,   Serial}, Port),
	put({serial, Port},   Serial),
	
	% signal back some useful mapping
	try   ?SERVER ! {driver, Serial, Port, self()}
	catch _:_ -> noop
	end,
	loop_handler(Port).


%% =====================
%% IFK DRIVER loop
%% =====================


loop_handler(Port) ->
	receive
		{Port, {exit_status, _}} ->
			log(warning, "ifk_driver: driver crashed/could not load"),
			handle_crashed_driver(Port),
			exit(crashed);
			
		{Port, {data, Data}} ->
			Decoded = binary_to_term(Data),
			%%base:ilog(?MODULE, "loop_handler: decoded msg[~p]~n", [Decoded]),
			publish(Decoded);
		
		Msg ->
			log(debug, "ifk_driver: unknown msg: ", [Other])
	end,
	loop_handler(Port).


publish({MsgType, Msg}) ->
	?SWITCH:publish(phidgets, {MsgType, Msg});

publish(Unknown) ->
	log(debug, "ifk_driver: unknown msg from driver: ", [Unknown]).



%% =======================
%% CRASHED DRIVER RECOVERY
%% =======================

handle_crashed_driver(Port) ->
	try  ?SERVER ! {crashed, Port}
	catch _:_ -> noop
	end.



%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

log(Severity, Msg) ->
	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?SWITCH:publish(log, {?SERVER, {Severity, Msg, Params}}).

%clog(Ctx, Sev, Msg) ->
%	?SWITCH:publish(log, {Ctx, {Sev, Msg, []}}).

%clog(Ctx, Sev, Msg, Ps) ->
%	?SWITCH:publish(log, {Ctx, {Sev, Msg, Ps}}).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  CONFIG  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%% @doc List of parameters which cannot be customized
%%
%% @spec blacklist() -> list()
%%
blacklist() ->
	[].

%% @doc List of default parameters for the module
%%
%% @spec defaults() -> list()
%%
defaults() ->
	[
	 {ifk.driver.debug,       optional, atom,   false}
	,{ifk.driver.home.path,   optional, string, "/usr/bin/"}
	,{ifk.driver.normal.name, optional, string, "pem_drv_ifk"}
	,{ifk.driver.debug.name,  optional, string, "pem_drv_ifk_debug"}
	 ].

%% @doc List of descriptions for the module's defaults
%%
descriptions() ->
	[
	 {ifk.driver.debug,       "Debug flag (true|false) for port driver"}
	,{ifk.driver.home.path,   "Home path for drivers (with trailing slash) (e.g. /usr/bin/)"}
	,{ifk.driver.normal.name, "Filename of normal (default) driver"}
	,{ifk.driver.debug.name,  "Filename of debug version of driver"}
	 ].



