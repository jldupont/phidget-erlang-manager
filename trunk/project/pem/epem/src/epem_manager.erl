%% Author: Jean-Lou Dupont
%% Created: 2009-06-18
%% Description: TODO: Add description to manager
%%
%% MESSAGES GENERATED on the Reflector:
%% ====================================
%% 
%% {phidgetdevice, {{Serial, Type, state}, {date(), time(), now()}}},
%%
%% SUBSCRIPTIONS:
%%
%%  none
%%

-module(epem_manager).

-define(SERVER, manager).
-define(BUSSES, [log, sys, clock]).
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
		start_link/1,
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
	start_link([]).

start_link(Args) ->
	
	{debug, Debug}=base:kfind(debug, Args,false),
	DrvPath = base:pole(Debug, true, false, ?DRV_MNG_DEBUG, ?DRV_MNG),			
	LD = [{driver_path, DrvPath}],
	NArgs = lists:append(Args, LD),
	

	Pid = spawn_link(?MODULE, loop, []),
	register( ?MODULE, Pid ),
	?MODULE ! {args, NArgs},
	{ok, Pid}.


stop() ->
    ?MODULE ! stop.


start_drv(DrvPath) ->
	Pid = spawn(?MODULE, mng_drv, [DrvPath]),
	register(mng_drv, Pid),
	ok.


mng_drv(ExtPrg) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
    loop_drv(Port).



%% ====================================================================
%% Module Server
%% ====================================================================

loop() ->
	receive
		
		%%%% CONFIGURATION MANAGEMENT
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		%%% --------------------
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
		
		
		%% Send the 'ready' signal
		{args, Args} ->
			put(args, Args),
			{driver_path, DrvPath} = base:kfind(driver_path, Args),
			put(driver_path, DrvPath),
			switch:publish(manager, ready, self()),
			start_drv(DrvPath);
		
		{driver, crashed} ->
			log(warning, "manager: driver crashed~n"),
			DriverPath = get(driver_path),
			start_drv(DriverPath);

		stop ->
			exit(normal);
		
		Error ->
			base:elog(?MODULE, "unsupported message [~p]~n",[Error]),
			Error
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
	do_app_ready();


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
	log(warning, "app: Unexpected message: ", [Other]).



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
			M = {Msg, {date(), time(), now()}},
			?SWITCH:publish(phidgets, {MsgType, M})
	
	end,
	loop_drv(Port).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

log(Severity, Msg) ->
	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?SWITCH:publish(log, {?SERVER, {Severity, Msg, Params}}).

%clog(Ctx, Sev, Msg) ->
%	?SWITCH:publish(log, {Ctx, {Sev, Msg, []}}).

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
	
	,{manager.driver.home.path,   optional, string, "/usr/bin"}
	,{manager.driver.normal.name, optional, string, "pem_drv_mng"}
	,{manager.driver.debug.name,  optional, string, "pem_drv_mng_debug"}
	 ].

%% @doc List of descriptions for the module's defaults
%%
descriptions() ->
	[
	 {manager.driver.debug,       "Debug flag (true|false) for drv_mng driver"}
	,{manager.driver.home.path,   "Home path for drv_mng driver"}
	,{manager.driver.normal.name, "Filename of normal (default) drv_mng driver"}
	,{manager.driver.debug.name,  "Filename of debug version of drv_mng driver"}
	 ].



