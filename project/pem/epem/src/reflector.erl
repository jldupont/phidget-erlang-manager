%% Author: Jean-Lou Dupont
%% Created: 2009-06-18
%% Description: Publishes messages from a Source
%%              to the target Destination subscriber(s).
%%
%% {subscribe, Msgtype}
%%
%%

-module(reflector).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/0,
	 stop/0,
	 subscribe/2
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
		 loop/0,
		 rpc/1,
		 publish/1,
		 do_publish/3
		 ]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
subscribe(Client, Msgtype) ->
	rpc({subscribe, Client, Msgtype}).


rpc(Q) ->
	?MODULE ! {self(), Q},
	receive
		{?MODULE, Reply} ->
			Reply
	end.

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/0
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start() ->
	Pid = spawn(fun() -> loop() end),
	register( ?MODULE, Pid ),
	ok.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
    rpc({stop}).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ==========
%% Func: loop
%% ==========
loop() ->
	receive
		{_From, {stop}} ->
			exit(ok);

		%% SUBSCRIBE command
		{From, {subscribe, Msgtype}} ->
			Liste = get(Msgtype),
			New_liste = Liste ++ [From],
			put(Msgtype, New_liste),
			ok;
		
		%% Message publication
		{_From, {Msgtype, Msg}} ->
			publish({Msgtype, Msg});

		Error ->
			Error
	end,
	loop().

publish(M) ->
	{Msgtype, Msg} = M,
	Liste = get(Msgtype),
	do_publish(Liste, Msgtype, Msg).


do_publish([], _, _) ->
	ok;


do_publish(Liste, Msgtype, Msg) ->
	[Current|Rest] = Liste,
	
	%% TODO on error, remove Pid of the target
	Current ! {Msgtype, Msg},
	do_publish(Rest, Msgtype, Msg).



