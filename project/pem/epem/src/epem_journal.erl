%% Author: Jean-Lou Dupont
%% Created: 2009-09-13
%% Description: Database journaling
%%
-module(epem_journal).

-define(DB, epem_db).

-define(SERVER,  journal).
-define(SWITCH,  epem_hwswitch).
-define(BUSSES,  [sys, clock, phidgets]).
-define(TOOLS,   mswitch_tools).
-define(CTOOLS,  mswitch_ctools).
-define(MSWITCH, mswitch).


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
		,descriptions/0
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
			maybe_start_db(),
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
		{mswitch, From, notif, Payload} ->
			handle({mswitch, From, notif, Payload});

		
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
	
		Other ->
			log(warning, "journal server: unexpected message: ", [Other])
	end,
	loop().



%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


handle({mswitch, _From, sys, _}) ->
	noop;

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

handle({hwswitch, _From, sys, _Msg}) ->
	not_supported;

handle({hwswitch, _From, phidgets, {phidgetdevice, Msg}}) ->
	handle_pd(Msg);

handle({hwswitch, _From, phidgets, {din, Msg}}) ->
	handle_io(din, Msg);

handle({hwswitch, _From, phidgets, {dout, Msg}}) ->
	handle_io(dout, Msg);


handle(Other) ->
	log(warning, "mswitch_bridge: Unexpected message: ", [Other]).





%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


maybe_start_db() ->
	State=get_state(),
	todo.

get_dsn() ->
	get(journal.dsn).



handle_pd(Msg) ->
	{Year, Month, Day}=date(),
	{Hour, Min, Sec}=time(),
	{Serial, Type, Status}=Msg,
	Ts=base:format_timestamp(Year, Month, Day, Hour, Min, Sec),
	
	%{{Serial, Type, Status}, {{Year, Month, Day}, {Hour, Min, Sec}, _}} = Msg,
	%%base:ilog(?MODULE, "pd: Serial[~p] Type[~p] Status[~p]~n",[Serial, Type, atom_to_list(Status)]),
	Conn = base:getvar(db_conn, undefined),
	handle_pd(Conn, Serial, Type, atom_to_list(Status), Ts).


handle_pd(undefined, _Serial, _Type, _Status, _Ts) ->
	log(error, "no db connection"),
	db_conn_err;
	
	
handle_pd(Conn, Serial, Type, Status, Ts) when is_pid(Conn) ->
	%%base:ilog(?MODULE, "inserting pd: Serial[~p] Type[~p] Status[~p]~n", [Serial, Type, Status]),
	%%insert_device_update(Conn, Serial, Type, Version, Name, Label, State, Ts) ->
	case db:insert_device_update(Conn, Serial, Type, " ",     " ",   " ",    Status, Ts) of
		ok -> ok;
		_ ->
			db:close(Conn),
			put(db_conn, undefined),
			error
	end;

handle_pd(_Conn, _Serial, _Type, _Status, _Ts) ->
	log(error, "invalid db connection").


handle_io(IOType, Msg) ->
	%%io:format("handle_io: msg:  ~p~n", [Msg]),
	%M = {Msg, {date(), time(), now()}},
	{Year, Month, Day}=date(),
	{Hour, Min, Sec}=time(),
	{Serial, Index, Value}=Msg,
	Ts=base:format_timestamp(Year, Month, Day, Hour, Min, Sec),	
	%{{Serial, Index, Value}, {{Year, Month, Day}, {Hour, Min, Sec}, _MegaSecs}}=Msg,
	%%base:ilog(?MODULE, "io: Serial[~p] IOType[~p] Index[~p] Value[~p]~n", [Serial, IOType, Index, Value]),
	Conn = base:getvar(db_conn, undefined),
	handle_io(IOType, Conn, Serial, Index, Value, Ts).

handle_io(_IOType, undefined, _Serial, _Index, _Value, _Ts) ->
	log(error, "no db connection"),
	db_conn_err;

handle_io(IOType, Conn, Serial, Index, Value, Ts) when is_pid(Conn) ->
	log(debug, "inserting event~n"),
	case db:insert_event_update(Conn, Serial, IOType, Index, Value, Ts) of
		ok ->
			ok;
		_ ->
			db:close(Conn),
			put(db_conn, undefined),
			error
	end;

handle_io(_IOType, _Conn, _Serial, _Index, _Value, _Ts) ->
	log(error, "invalid db connection").



%% ===============
%% LOCAL FUNCTIONS
%% ===============

try_start_db() ->
	ConnState=?TOOLS:getvar(db_conn, undefined),
	
	case ConnState of
		undefined ->
			case open_db() of
				connected ->
					log(debug, "journal: db connected"),
					connected;
				_ ->
					log(debug, "journal: cannot connect to db"),
					not_connected
			end;
		_ -> ok
	end.


open_db() ->
	open_db(get_dsn()).

open_db(undefined) ->
	cant_connect;

open_db(DSN) ->
	case db:open(DSN) of
		{ok, Conn} ->
			put(db_conn, Conn),
			db:create_tables(Conn),
			connected;
		_ ->
			put(db_conn, undefined),
			cant_connect
	end.



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
	 {journal.dsn,   optional, nstring, "epem"}
	,{journal.state, optional, atom,    working}
	,{journal.debug, optional, atom,    true}
	 ].

descriptions() ->
	[
	 {journal.dsn,   "ODBC DSN"}
	,{journal.state, "Journal state: working | suspended"}
	,{journal.debug, "Debug mode for journal (true | false)"}
	 ].