%% Author: Jean-Lou Dupont
%% Created: 2009-06-18
%% Description: Phidget Manager
%%
%% @doc
%%
%% = Messages on HWSWITCH =
%% phidgets.{phidgetdevice, {{Serial, Type, state}, {date(), time(), now()}}},
%% 

-module(epem_manager).

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
			restart_drv();
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		%%% --------------------
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
		

		{driver, crashed} ->
			log(warning, "manager: driver crashed... restarting~n"),
			restart_drv();

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

restart_drv() ->
	stop_drv(),
	start_drv().




mng_drv(ExtPrg) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
	log(info, "manager: driver started on port: ", [Port]),
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
			Decoded = binary_to_term(Data),
			%% Decoded:  {Msgtype, Msg}
			%%            Atom     Tuple
			{MsgType, Msg} = Decoded,
			%%M = {Msg, {date(), time(), now()}},
			?SWITCH:publish(phidgets, {MsgType, Msg});
	
		%% /dev/null
		_ ->
			noop
	end,
	loop_drv(Port).


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



