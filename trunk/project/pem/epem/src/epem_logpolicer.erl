%% Author: Jean-Lou Dupont
%% Created: 2009-09-03
%% Description: Log policer
%%
%% = Policies =
%%
%% {acc, day} ==> Accumulate on a daily basis
%%
%%
-module(epem_logpolicer).

-define(APP,    epem).
-define(SERVER, logpolicer).
-define(SWITCH, epem_hwswitch).
-define(BUSSES, [sys, clock, log]).
-define(CTOOLS, mswitch_ctools).
-define(TOOLS,  mswitch_tools).
-define(LOG,    epem_log).

-define(DAY,    24*60*60*1000*1000).  % in microseconds
-define(HOUR,   60*60*1000*1000).     % in microseconds
-define(MIN,    60*1000*1000).        % in microseconds
-define(MIN5,   5*60*1000*1000).      % in microseconds

%%
%% API Exported Functions
%%
-export([
		 start_link/0
		 ,stop/0
		 ,get_server/0, get_busses/0
		
		 %% keep compilation warning free
		,loop/0
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


start_link() ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	{ok, Pid}.


stop() ->
	try ?SERVER ! stop, ok
	catch _:_ -> {error, cannot_stop}
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
			handle({hwswitch, From, Bus, Msg});
	
		Other ->
			log(warning, "logpolicer: unexpected message: ", [Other])
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



handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	?CTOOLS:do_config(?SWITCH, ?SERVER, VersionInForce);

handle({hwswitch, _From, sys, _Msg}) ->
	not_supported;


handle({hwswitch, _From, log, {Context, {Severity, Msg, Params}}}) ->
	'l1.filter'(Context, Severity, Msg, Params);
	

handle(Other) ->
	log(warning, "logpolicer: Unexpected message: ", [Other]).



%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  FILTERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------


%%%% L1
%%%%


%'l1.filter'(app.ready, Severity, Msg, Params) ->
%	'log.once.per.run'(Severity, Msg, Params);

'l1.filter'(Context, Severity, Msg, Params) ->
	'l2.filter'(Context, Severity, Msg, Params).

%%%% L2
%%%%

'l2.filter'(Context, Severity, Msg, Params) ->
	FlagVarName=?TOOLS:make_atom_from_list([logpolicer,'.',Severity]),
	Flag=get(FlagVarName),
	%io:format("maybe_dolog: fn [~p] f[~p] s[~p] m[~p]~n", [FlagVarName, Flag, Severity, Msg]),
	'l2.filter'(Flag, Context, Severity, Msg, Params).

'l2.filter'(false, _Context, _Severity, _Msg, _Params) -> blocked;
'l2.filter'(true, Context, Severity, Msg, Params) ->
	'l3.filter'(Context, Severity, Msg, Params);
'l2.filter'(undefined, Context, Severity, Msg, Params) ->
	'l3.filter'(Context, Severity, Msg, Params).


%%%% L3
%%%%


'l3.filter'(Context, Sev, Msg, Params) ->
	PolicyVarName=?TOOLS:make_atom_from_list([logpolicer,'.',Context]),
	Policy=get(PolicyVarName),
	%io:format("Policy {Context:~p, Policy:~p}~n", [Context, Policy]),
	'l3.filter'(Policy, Context, Sev, Msg, Params).


%% No policy then log.
'l3.filter'(undefined, _Context, Severity, Msg, Params) ->
	log(Severity, Msg, Params);

'l3.filter'({acc, min5}, Context, Sev, Msg, Params) ->
	CC=get({context, Context}),
	filter_acc(?MIN5, CC, Context, Sev, Msg, Params);


'l3.filter'({acc, min}, Context, Sev, Msg, Params) ->
	CC=get({context, Context}),
	filter_acc(?MIN, CC, Context, Sev, Msg, Params);

'l3.filter'({acc, day}, Context, Sev, Msg, Params) ->
	CC=get({context, Context}),
	filter_acc(?DAY, CC, Context, Sev, Msg, Params);

'l3.filter'({acc, hour}, Context, Sev, Msg, Params) ->
	CC=get({context, Context}),
	filter_acc(?HOUR, CC, Context, Sev, Msg, Params);


%% Log only once per application run (reload constitutes a new 'run')
'l3.filter'({run, once}, Context, Sev, Msg, Params) ->
	CC=get({context, Context}),
	CurrentPid=os:getpid(),	
	case CC of
		undefined ->
			put({context, Context}, {pid, CurrentPid}),
			log(Sev, Msg, Params);
		{pid, SavedPid} -> 
				case CurrentPid==SavedPid of
					true  -> 'dont.log.again';
					false ->
						put({context, Context}, {pid, CurrentPid}),
						log(Sev, Msg, Params)
				end
	end;


'l3.filter'(_, _Context, Sev, Msg, Params) ->
	log(Sev, Msg, Params).


%% First time around... setup a context
filter_acc(Ms, undefined, Context, Sev, Msg, Params) ->
	put({context, Context}, {Ms, now(), 1}),
	log(Sev, Msg, Params);

filter_acc(_, CC, Context, Sev, Msg, Params) ->
	{Ms, PeriodStart, Count} = CC,
	TimeDiff=timer:now_diff(now(), PeriodStart),
	io:format("{Ms: ~p, Context: ~p, Msg:~p, PeriodStart:~p, Count:~p, TimeDiff:~p}~n~n", [Ms, Context, Msg, PeriodStart, Count, TimeDiff]),
	case TimeDiff > Ms of
		true ->
			% start new period
			put({context, Context}, {Ms, now(), 1}),
			log(Sev, "Accumulated count {Context, Count}", [[Context, Count]]),
			log(Sev, Msg, Params);
		_    ->
			% in period, just accumulate hit
			put({context, Context}, {Ms, PeriodStart, Count+1})
	end.
	

	

%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------




%log(Severity, Msg) ->
%	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?LOG:policed_log(Severity, Msg, Params).



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
	   {logpolicer.debug,    optional, atom, true}	 
	  ,{logpolicer.critical, optional, atom, true}
	  ,{logpolicer.info,     optional, atom, true}
	
	 ,{logpolicer.app.ready,                optional, taa, {run, once}}
	
	 ,{logpolicer.manager.driver.pathcheck, optional, taa, {acc, hour}}
	 ,{logpolicer.ifk.driver.crashed,       optional, taa, {acc, hour}}
	 ].


descriptions() ->
	[
	   {logpolicer.debug,    "Allow 'debug' log entries (true|false)"}	 
	  ,{logpolicer.critical, "Allow 'critical' log entries (true|false)"}
	  ,{logpolicer.info,     "Allow 'info' log entries (true|false)"}
	
	 ,{logpolicer.app.ready, "Policy for 'sys.app.ready' message"} 
	 ,{logpolicer.manager.driver.pathcheck, "Policy for 'manager.driver.patchcheck' message"}
	 ,{logpolicer.ifk.driver.crashed, "Policy for 'ifk.driver.crashed' message"}
	 ].

%% Contextual log messages
%% =======================
%%
%% > manager.driver.open_attempt
%% > manager.drv.crashed
%% > manager.driver.rxerror
%% > manager.driver.pathcheck

