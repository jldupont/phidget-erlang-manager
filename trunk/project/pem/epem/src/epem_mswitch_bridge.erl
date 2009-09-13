%% Author: Jean-Lou Dupont
%% Created: 2009-09-13
%% Description: Serves to bridge selected messages to the MSWITCH bus
%%
%%
-module(epem_mswitch_bridge).

-define(APPNAME, epem).
-define(SERVER,  mswitch_bridge).
-define(SWITCH,  epem_hwswitch).
-define(BUSSES,  [sys, clock, phidgets]).
-define(CTOOLS,  mswitch_ctools).
-define(MSWITCH, mswitch).

-define(MSGS, [{phidgets, phidgetdevice} 
			   ,{phidgets, din}
			   ,{phidgets, dout}
			  ]
	   ).

%%
%% API Functions
%%
-export([
		 start_link/0
		,stop/0
		,get_server/0
		,get_busses/0
		]).

%%
%% LOCAL Functions
%%
-export([
		 loop/0
		 ]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).


%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  Management  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.


start_link()->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	{ok, Pid}.

stop() ->
	try 
		?SERVER ! stop,  ok
	catch
		_:_ ->	{error, cannot_stop}
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
loop() ->
	receive
		
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			maybe_bridge(From, Bus, Msg),
			handle({hwswitch, From, Bus, Msg});
	
		Other ->
			log(warning, "mswitch_bridge: unexpected message: ", [Other])
	end,
	loop().

%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

 
handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;


handle({hwswitch, _From, sys, suspend}) ->
	set_state(suspended);

handle({hwswitch, _From, sys, resume}) ->
	set_state(working);

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	put(config.version.inforce, VersionInForce);

handle({hwswitch, _From, sys, app.ready}) ->
	do_app_ready();


handle({hwswitch, _From, sys, _Msg}) ->
	not_supported;



handle(Other) ->
	log(warning, "mswitch_bridge: Unexpected message: ", [Other]).


%% @doc Bridge, if required, the HWSWITCH messages
%%		to the MSWITCH busses.
%%
maybe_bridge(_From, Bus, Msg) when is_tuple(Msg) ->
	Type=erlang:hd(erlang:tuple_to_list(Msg)),
	Target={Bus, Type},
	case lists:member(Target, ?MSGS) of
		true ->	?MSWITCH:publish(Bus, Msg);
		_    -> noop
	end;

maybe_bridge(_From, Bus, Msg) when is_atom(Msg) ->
	Target={Bus, Msg},
	case lists:member(Target, ?MSGS) of
		true ->	?MSWITCH:publish(Bus, Msg);
		_    -> noop
	end;

maybe_bridge(_From, _Bus, _Msg) ->
	unsupported.



%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


do_app_ready() ->
	State=get_state(),
	maybe_do_app_ready(State).


maybe_do_app_ready(working) ->
	{Pid, _}=string:to_integer(os:getpid()),
	?MSWITCH:publish(sys, {app.ready, ?APPNAME, Pid});
	
maybe_do_app_ready(_) ->
	noop.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
set_state(State) ->
	put(state, State).

get_state() ->
	State=get(state),
	case State of
		undefined -> working;   %start in 'working' state
		working   -> working;
		_         -> suspended
	end.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%%log(Severity, Msg) ->
%%	log(Severity, Msg, []).

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
	[].

