%% Author: Jean-Lou Dupont
%% Created: 2009-09-10
%% Description: 
-module(epem).

-define(SERVER, epem).
-define(BUSSES, [log, clock, sys]).
-define(RPC,    epem_rpc).
-define(CTOOLS, mswitch_ctools).
-define(SWITCH, epem_hwswitch).


%%
%% Exported Functions
%%
-export([
		 start_link/0
		,stop/0
		,get_server/0
		,get_busses/0
		
		 ,loop/0
		 ]).

%%
%% API functions
%%
-export([
		 ]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.



%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  MANAGEMENT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------

start_link() ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	{ok, Pid}.


stop() -> 
	try ?SERVER ! stop
	catch _:_ -> noop
	end.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  SERVER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
loop() ->
	receive
		
		stop ->
			exit(normal);
		
		%% RPC bridge
		%%
		%% Messages ending up here are (usually ;-)
		%% sent through using the 'rpc' function.
		%%
		{rpc, ReplyTo, {FromNode, ReplyContext, Q}} ->
			?RPC:handle_rpc(ReplyTo, FromNode, ReplyContext, Q);

		%%%% CONFIGURATION MANAGEMENT
		%%
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
	
		%%% LOCAL SWITCH RELATED %%%
		%%% --------------------
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
		
		_Other ->
			log(critical, "epem: unhandled message: ", [_Other])

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
	todo;


handle({hwswitch, _From, sys, suspend}) ->
	set_state(suspended);

handle({hwswitch, _From, sys, resume}) ->
	set_state(working);

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	put(config.version.inforce, VersionInForce);


%% A module advertises its configuration version
handle({hwswitch, _From, sys, {mod.config, Module, Version}}) ->
	put({mod.version, Module}, Version);


handle({hwswitch, _From, sys, _Msg}) ->
	not_supported;


handle(Other) ->
	log(warning, "app: Unexpected message: ", [Other]).



%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------


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

