%% Author: Jean-Lou Dupont
%% Created: 2009-06-18
%% Description: TODO: Add description to manager
-module(manager).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

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
		 loop/0,
		 loop_drv/1,
		 init_drv/2,
		 send_to_reflector/1,
		 send_to_reflector/2
		 ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
start_link() ->
	start_link("").

start_link(Param) ->
	Pid = spawn(fun() -> loop() end),
	register( ?MODULE, Pid ),
	error_logger:info_msg("manager:start_link: PID[~p]~n", [Pid]),
	Pid_drv = spawn_link(?MODULE, init_drv, ["/usr/bin/pem_drv_mng_debug", Param]),
	error_logger:info_msg("manager:start_link: PID DRV[~p]~n", [Pid_drv]),
	{ok, Pid}.

init_drv(ExtPrg, Param) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg++" "++Param}, [{packet, 2}, binary, exit_status]),
    loop_drv(Port).


%% --------------------------------------------------------------------
%% Func: stop/0
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
    ?MODULE ! stop.

%% ====================================================================
%% Internal functions
%% ====================================================================

loop() ->
	receive
		stop ->
			exit(ok);
		
		Error ->
			error_logger:warning_msg("manager:loop: unsupported message"),
			Error
	end,
	loop().

%% Message loop for receiving messages from pem_drv_mng
%% ====================================================
loop_drv(Port) ->
	receive
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


%% TODO count error etc.
send_to_reflector(undefined, _) ->
	error_logger:warning_msg("manager:send_to_reflector: reflector not found~n"),
	ok;

send_to_reflector(Reflector, M) ->
	error_logger:info_msg("manager:send_to_reflector: Msg[~p]~n", [M]),
	Self = self(),
	try Reflector ! {Self, M} of
		
		%% we're echo'ed back the message if everything is OK
		{Self, M} ->
			ok;
		Code ->
			error_logger:warning_msg("manager:send_to_reflector: error sending to reflector, code[~p]", [Code])
	catch
		_:_ -> 
			error_logger:warning_msg("manager:send_to_reflector: error sending~n"),
			ok
	end.
