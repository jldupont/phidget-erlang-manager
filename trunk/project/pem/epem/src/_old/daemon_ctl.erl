%% Author: Jean-Lou Dupont
%% Created: 2009-06-23
%% Description: Control for the daemon
%%
%% Duties:
%% =======
%%
%%  - Writes assigned port to CTL file
%%  - Responds to client messages:
%%    - what_pid
%%    - do_exit
%%
%% SUBSCRIPTIONS:
%% ==============
%%  
%%  {assignedport, Port}         %% Write Port to CTL file
%%  {daemonized}                 %% Change state machine Context
%%
%%  {management,  X}             %% Client context: status from sending activity to DAEMON
%%
%%  {from_client, M}             %% Daemon context: Rx messages from CLIENT
%%
%%
%% MESSAGE GENERATED:
%% ==================
%%
%% {control,     canstart}
%% {control,     stop_sent_ok}
%% {control,     stop_sent_error}
%% {control,     daemon_not_found}
%%
%% {daemon_pid,  Pid}            %% Client context: daemon pid found
%% {daemon_exit, Pid}            %% Daemon context: client asked for daemon to exit
%%
%%
%% MESSAGES RECEIVED from PEM_ADMIN (daemon_client):
%%
%%  what_pid  
%%  do_exit
%%
%% MESSAGES SENT TO THE CLIENT through daemon_server:
%%
%% {pid, Pid}
%% 

-module(daemon_ctl).

%% Timeout for communicating
%% with the daemon
-define(TIMEOUT, 2000).

%% Reflector subscriptions
-define(SUBS, [assignedport, management, daemonized, from_client]).


%% =============================
%% API Exported Functions
%% =============================
-export([
		 start/0,
		 start_link/0,
		 start_link/1
		 ]).

%%
%% LOCAL Exported Functions
%%
-export([
		 loop/0,
		 hevent/1,
		 hcevent/3
		 ]).

%% =======================================
%% API Functions
%% =======================================
start() ->
	Pid = spawn(?MODULE, loop, []),
	register(daemon_ctl, Pid),
	Pid ! started,
	{ok, Pid}.
	
start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register(daemon_ctl, Pid),
	Pid ! started,
	{ok, Pid}.

start_link(Args) ->
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	?MODULE ! {args, Args},
	{ok, Pid}.


	

%% ====================================================================================
%% Local Functions
%% ====================================================================================




%% ================================================================
%% MAIN LOOP
%% ================================================================
loop() ->
	receive
		%% Send the 'ready' signal
		{args, Args} ->
			put(args, Args),
			{root, Root} = base:kfind(root, Args),
			base:send_ready_signal(reflector, Root, {});

		{from_reflector, subscribed} ->
			ok;
		
		
		started ->
			reflector:subscribe(daemon_ctl, ?SUBS),
			put(state,  started);

		%% SAVE the assigned port to the CTL file
		{assignedport, Port} ->
			base:saveport(Port);
		
		{from_client, Msg} ->
			ok;
		
		{daemonized} ->
			hevent(daemonized);
		
		{management, Msg} ->
			hevent({management, Msg});
		
		Other ->
			base:ilog(?MODULE, "received [~p]~n", [Other])
	
	after ?TIMEOUT ->

		reflector:sync_to_reflector(?SUBS),
		hevent(timeout)
	
	end,
	loop().


hevent(E) ->
	Context = get(context),
	State   = get(state),
	base:ilog(?MODULE, "hevent: Context[~p] State[~p]~n",[Context, State]),	
	hcevent(Context, State, E).
	

%% DAEMON STARTED
%% ==============

hcevent(_, daemonized) ->
	put(context, daemon);




%% Management channel found: ask for PID to running daemon
hcevent(client, wait_management, start, {management, open}) ->
	reflector:send_sync(self(), to_daemon, {asked_pid, what_pid}, ?SUBS),  %% {to_daemon, {asked_pid, what_pid}}
	put(state, wait_pid);

%% Couldn't reach a running daemon
hcevent(client, wait_management, start, timeout) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, canstart, ?SUBS);           %% {control, canstart}

%% We received a PID; that means that a daemon is effectively running
%% and we can't be starting a new one!
hcevent(client, wait_pid, start, {from_daemon, {pid, Pid}}) ->
	put(state, daemon_present),
	reflector:send_sync(self(), daemon_pid, Pid, ?SUBS);             %% {daemon_pid, Pid}
	
%% Timed-out whilst waiting for a PID from the daemon...
%% assume the "best case" i.e. no daemon running
hcevent(client, wait_pid, start, timeout) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, canstart, ?SUBS);           %% {control, canstart}

%% Out "what_pid" was successfully sent... good
hcevent(client, wait_pid, start, {management, {txok, _MsgId} }) ->
	ok;

%% Error in transmitting to daemon... assume "best case"
hcevent(client, wait_pid, start, {management, {_, _MsgId} }) ->
	put(state, daemon_not_found),
	reflector:send_sync(self(), control, canstart, ?SUBS);           %% {control, canstart}



%%%%CATCH-ALL%%%%
hcevent(client, State, start, Event) ->
	base:elog(?MODULE, "hcevent: INVALID PATTERN, Client: State[~p] Command[Start] Event[~p]~n", [State, Event]);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%               DAEMON SIDE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% We don't need the timer in this context.
hcevent(daemon, _, _, timeout) ->
	ok;

%% The management client is asking us to exit daemon state...
hcevent(daemon, started, _, {from_client, do_exit}) ->
	Pid = os:getpid(),
	reflector:send_sync(self(), daemon_exit, Pid, ?SUBS);                   %% {daemon_exit, Pid}


%% The management client is inquiring about our PID...
hcevent(daemon, started, _, {from_client, what_pid}) ->
	Pid = os:getpid(),
	reflector:send_sync(self(), to_client, {pid, Pid}, ?SUBS);              %% {to_client, {pid, Pid}}

%%%%CATCH-ALL%%%%
hcevent(daemon, State, Command, Event) ->
	base:elog(?MODULE, "hcevent: INVALID PATTERN, Daemon: State[~p] Command[~p] Event[~p]~n", [State, Command, Event]);


hcevent(Context, State, Command, Event) ->
	base:elog(?MODULE, "hcevent: INVALID PATTERN, Context[~p] State[~p] Command[~p] Event[~p]~n", [Context, State, Command, Event]).



