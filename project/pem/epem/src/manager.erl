%% Author: Jean-Lou Dupont
%% Created: 2009-06-18
%% Description: TODO: Add description to manager
-module(manager).

%% --------------------------------------------------------------------
%% MACROS
%% --------------------------------------------------------------------
-define(DRV_MNG,       "pem_drv_mng").
-define(DRV_MNG_DEBUG, "pem_drv_mng_debug").

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start_link/0,
	 start_link/1,
	 stop/0
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
		 start/1,
		 loop/0,
		 loop_drv/1,
		 start_drv/1,
		 mng_drv/1,
		 send_to_reflector/1,
		 send_to_reflector/2
		 ]).

%% ====================================================================!
%% API functions
%% ====================================================================!
start_link(Args) ->
	base:elog(?MODULE, "start_link: Args[~p]~n",[Args]),
	
	Debug = base:is_debug(Args),
	case Debug of
		true ->
			Driver = ?DRV_MNG_DEBUG;
		_Other ->
			Driver = ?DRV_MNG
	end,
	start(Driver).

start_link() ->
	start(?DRV_MNG).

stop() ->
    ?MODULE ! stop.


start(DrvPath) ->
	Pid = spawn_link(?MODULE, loop, []),
	register( ?MODULE, Pid ),
	?MODULE ! {driver, dostart, DrvPath},
	{ok, Pid}.
	

start_drv(DrvPath) ->
	_Pid_drv = spawn(?MODULE, mng_drv, [DrvPath]),
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
		
		{driver, dostart, DrvPath} ->
			put(driver_path, DrvPath),
			start_drv(DrvPath);
		
		{driver, crashed} ->
			error_logger:error_msg("~p: driver crashed~n", [?MODULE]),
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
			{Msgtype, Msg} = Decoded,
			M = {Msgtype, Msg, {date(), time(), now()}},
			send_to_reflector(M)
	
	end,
	loop_drv(Port).


send_to_reflector(M) ->
	Reflector = whereis(reflector),
	send_to_reflector(Reflector, M).


send_to_reflector(undefined, _) ->
	base:elog(?MODULE, "reflector not found~n"),
	ok;

send_to_reflector(Reflector, M) ->
	Self = self(),
	try Reflector ! {Self, M} of
		
		%% we're echo'ed back the message if everything is OK
		{Self, M} ->
			ok;
		Code ->
			base:elog(?MODULE, "error sending to reflector, code[~p]", [Code])
	catch
		X:Y -> 
			base:elog(?MODULE, "error sending [~p:~p]~n", [X,Y]),
			ok
	end.
