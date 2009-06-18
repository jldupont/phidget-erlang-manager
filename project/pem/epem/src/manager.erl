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
	 start/0,
	 start/1,
	 stop/0
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
		 loop/0,
		 loop_drv/1,
		 init_drv/2,
		 send_to_reflector/1
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
start() ->
	start("").

start(Param) ->
	Pid = spawn(fun() -> loop() end),
	register( ?MODULE, Pid ),
	spawn_link(?MODULE, init_drv, ["/usr/bin/pem_drv_mng", Param]),
	ok.

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
			Error
	end,
	ok.

loop_drv(_Port) ->
	receive
		{Port, {data, Data}} ->
			Decoded = binary_to_term(Data),
			send_to_reflector(Decoded)
	end,
	loop_drv(Port).


send_to_reflector(M) ->
	Reflector = whereis(reflector),
	
		
	ok.

%% TODO count error etc.
send_to_reflector(undefined, _) ->
	ok.

send_to_reflector(Reflector, M) ->
	try Reflector ! M of
		ok -> ok
	catch
		
	end.
