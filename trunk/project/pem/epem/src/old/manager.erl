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

-module(manager).

%% --------------------------------------------------------------------
%% MACROS
%% --------------------------------------------------------------------
-define(DRV_MNG,       "pem_drv_mng").
-define(DRV_MNG_DEBUG, "pem_drv_mng_debug").

-define(TIMEOUT,       2000).
-define(SUBS,          [daemonized]).

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start_link/1,
	 stop/0
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

%% ====================================================================!
%% API functions
%% ====================================================================!
start_link(Args) ->
	
	{debug, Debug}=base:kfind(debug, Args,false),
	DrvPath = base:pole(Debug, true, false, ?DRV_MNG_DEBUG, ?DRV_MNG),			
	LD = {driver_path, DrvPath},
	NArgs = lists:append(Args, LD),
	
	base:ilog(?MODULE, "start_link: Args[~p]~n",[NArgs]),

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
%% Internal functions
%% ====================================================================

loop() ->
	receive
		
		%% Send the 'ready' signal
		{args, Args} ->
			put(args, Args),
			{root, Root} = base:kfind(root, Args),
			{driver_path, DrvPath} = base:kfind(drv_path, Args),
			put(driver_path, DrvPath),
			base:send_ready_signal(reflector, Root, {});

		
		{from_reflector, subscribed} ->
			ok;
		
		%% Only start when we are actually in daemon mode
		{daemonized, _} ->
			DrvPath=get(driver_path),
			start_drv(DrvPath);
		
		
		%% Grab driver's path
		{driver, canstart, DrvPath} ->
			put(driver_path, DrvPath);
			
		
		{driver, crashed} ->
			base:elog(?MODULE, "driver crashed~n", []),
			DriverPath = get(driver_path),
			start_drv(DriverPath);

		stop ->
			exit(ok);
		
		Error ->
			base:elog(?MODULE, "unsupported message [~p]~n",[Error]),
			Error
	end,
	loop().

%% Message loop for receiving messages from pem_drv_mng
%% ====================================================
loop_drv(Port) ->
	receive
		
		% port driver has crashed... propagate failure
		{Port, {exit_status, _}} ->
			?MODULE ! {driver, crashed},
			exit(crashed);

		
		{Port, {data, Data}} ->
			Decoded = binary_to_term(Data),
			%% Decoded:  {Msgtype, Msg}
			%%            Atom     Tuple
			{MsgType, Msg} = Decoded,
			M = {Msg, {date(), time(), now()}},
			reflector:send(self(), MsgType, M)
	
	end,
	loop_drv(Port).

