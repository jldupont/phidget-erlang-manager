%% Author: Jean-Lou Dupont
%% Created: 2009-06-18
%% Description: Phidget Manager
%%
%% @doc
%%
%% = Messages on HWSWITCH =
%% phidgets.{phidgetdevice, {Serial, Type, state}},
%% 
%%
%% = Failure Modes =
%%
%% 1) Driver not installed  ==> no use logging every attempt
%% 2) Driver crashing       ==> rate limit?
%% 3) Intermittent crash    ==>
%%
%% Two events:
%% 1) Reload: log attempt/success/failure every time
%% 2) Retry (or start): log success
%%
-module(epem_manager).

-include_lib("kernel/include/file.hrl").


-define(SERVER_DRV, manager_driver).
-define(SERVER, manager).
-define(BUSSES, [sys, clock]).
-define(CTOOLS, mswitch_ctools).
-define(SWITCH, epem_hwswitch).


%% --------------------------------------------------------------------
%% MACROS
%% --------------------------------------------------------------------
-define(DRV_MNG,       "pem_drv_mng").
-define(DRV_MNG_DEBUG, "pem_drv_mng_debug").

-define(TIMEOUT,       2000).
-define(SUBS,          []).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
		start_link/0,
		stop/0
		
		,get_server/0
		,get_busses/0
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
		 loop/0,
		 loop_drv/1,
		 start_drv/1,
		 mng_drv/1
		 ]).

%% --------------------------------------------------------------------
%% Config Functions
%% --------------------------------------------------------------------
-export([
		 defaults/0,
		 blacklist/0
		,descriptions/0
		 ]).


%% ====================================================================!
%% API functions
%% ====================================================================!
start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register( ?SERVER, Pid ),
	{ok, Pid}.


stop() ->
    ?MODULE ! stop.



%% ====================================================================
%% Module Server
%% ====================================================================

loop() ->
	receive
		
		%%%% CONFIGURATION MANAGEMENT
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config),
			log(info, "manager: restarting driver"),
			maybe_restart();
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		%%% --------------------
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
		
		{driver, {phidgets, {MsgType, Msg}}} ->
			handle_driver_msg(MsgType, Msg);

		{driver, crashed} ->
			handle_crashed_drv();

		Error ->
			log(error, "manager: unsupported message: ", Error)

	end,
	loop().


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HWSWITCH  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

handle({hwswitch, _From, clock, {tick.min, Count}}) ->
	put(clock.min, Count),
	maybe_restart(),
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;

handle({hwswitch, _From, sys, app.ready}) ->
	start_drv();


handle({hwswitch, _From, sys, suspend}) ->
	noop;

handle({hwswitch, _From, sys, resume}) ->
	noop;

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	put(config.version.inforce, VersionInForce);


%% A module advertises its configuration version
handle({hwswitch, _From, sys, {mod.config, Module, Version}}) ->
	put({mod.version, Module}, Version);


handle({hwswitch, _From, sys, _Msg}) ->
	not_supported;



handle(Other) ->
	log(warning, "manager: Unexpected message: ", [Other]).


%% ====================================================================
%% Driver API
%% ====================================================================

handle_driver_msg(_MsgType, _Msg) ->
	MinCount=get(clock.min),
	put(driver.state, {working, MinCount}).


handle_crashed_drv() ->
	clog(manager.drv.crashed, error, "manager: driver crashed"),
	MinCount=get(clock.min),
	put(driver.state, {crashed, MinCount}).



maybe_restart() ->
	stop_drv(),
	State=get(driver.state),
	maybe_restart(State).

maybe_restart({working, _MT}) ->
	already_working;

maybe_restart({crashed, _MT}) ->
	PathCheck=check_drv_path(),
	attempt_restart(PathCheck);

maybe_restart(undefined) ->
	PathCheck=check_drv_path(),
	attempt_restart(PathCheck).



attempt_restart({ok, _}) ->
	start_drv();

attempt_restart({error, Reason}) ->
	clog(manager.driver.pathcheck, error, "manager: driver path check error, reason: ", [Reason]);

attempt_restart(_) ->
	improbable.


%% @doc Starts the port driver iff not already started.
%%
start_drv() ->
	Port=get(driver.port),
	start_drv(Port).

start_drv(undefined) ->	real_drv_start();
start_drv(_) ->	already_started.			


real_drv_start() ->
	Pid = spawn(?MODULE, mng_drv, [get_drv_path()]),
	register(?SERVER_DRV, Pid),
	put(driver.port, Pid),
	{ok, Pid}.
	

stop_drv() ->
	Port=get(driver.port),
	stop_drv(Port).

stop_drv(undefined) -> noop;
stop_drv(Port) ->
	put(driver.port, undefined),
	try		port_close(Port)
	catch	_:_ -> noop
	end.



check_drv_path() ->
	Filename=get_drv_path(),
	file:read_file_info(Filename).

			


mng_drv(ExtPrg) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
	clog(manager.driver.open_attempt, info, "manager: attempting driver open on port: ", [Port]),
	
	%% IMPORTANT: The following has been commented out because
	%% erlang:open_port returns a valid "port" term *even* if the specified driver
	%% isn't available!
	%
	%case is_port(Port) of
	%	true ->
	%		clog(manager.driver.opened, info, "manager: driver started on port: ", [Port]);
	%	false ->
	%		clog(manager.driver.open_error, error, "manager: cannot start driver")
	%end,
	
    loop_drv(Port).



%% ====================================================================
%% Driver Server
%% ====================================================================

loop_drv(Port) ->
	receive
		
		% port driver has crashed... propagate failure
		{Port, {exit_status, _}} ->
			?SERVER ! {driver, crashed},
			exit(crashed);

		
		{Port, {data, Data}} ->
			handle_rx_drv(Data);
	
		%% /dev/null
		_ ->
			noop
	end,
	loop_drv(Port).


handle_rx_drv(Data) ->
	try
		Decoded = binary_to_term(Data),
		{MsgType, Msg} = Decoded,
		publish_drv_msg(MsgType, Msg)
	catch
		_:_ ->
			clog(manager.driver.rxerror, error, "manager: decode error, data: ", [Data])
	end.


publish_drv_msg(MsgType, Msg) ->
	?SWITCH:publish(phidgets, {MsgType, Msg}),
			
	%% We need to get some feedback to the main Server thread of this module
	?SERVER ! {driver, {phidgets, {MsgType, Msg}}}.

	
		
		
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.


%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------
get_drv_path() ->
	Home= get(manager.driver.home.path),
	Debug=get(manager.driver.debug),
	get_drv_path(Home, Debug).

get_drv_path(Home, true) ->
	Home++get(manager.driver.debug.name);

get_drv_path(Home, _) ->
	Home++get(manager.driver.normal.name).



%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

log(Severity, Msg) ->
	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?SWITCH:publish(log, {?SERVER, {Severity, Msg, Params}}).

clog(Ctx, Sev, Msg) ->
	?SWITCH:publish(log, {Ctx, {Sev, Msg, []}}).

clog(Ctx, Sev, Msg, Ps) ->
	?SWITCH:publish(log, {Ctx, {Sev, Msg, Ps}}).


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
	 %% Debug mode for external driver
	 {manager.driver.debug, optional, atom, false}
	
	,{manager.driver.home.path,   optional, string, "/usr/bin/"}
	,{manager.driver.normal.name, optional, string, "pem_drv_mng"}
	,{manager.driver.debug.name,  optional, string, "pem_drv_mng_debug"}
	 ].

%% @doc List of descriptions for the module's defaults
%%
descriptions() ->
	[
	 {manager.driver.debug,       "Debug flag (true|false) for drv_mng driver"}
	,{manager.driver.home.path,   "Home path for drv_mng driver with trailing slash (e.g. /usr/bin/)"}
	,{manager.driver.normal.name, "Filename of normal (default) drv_mng driver"}
	,{manager.driver.debug.name,  "Filename of debug version of drv_mng driver"}
	 ].



