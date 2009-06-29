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
-define(SUBS, [management_port, from_client]).


%% =============================
%% API Exported Functions
%% =============================
-export([
		 start_link/0,
		 start_link/1
		 ]).

%%
%% LOCAL Exported Functions
%%
-export([
		 loop/0,
		 hevent/1,
		 hcevent/2
		 ]).

%% =======================================
%% API Functions
%% =======================================
	
start_link() ->
	start_link([]).


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
			put(state, started),
			put(args, Args),
			switch:subscribe(daemon_ctl, ?SUBS);

		%% Send the 'ready' signal
		{switch, subscribed} ->
			put(state, subscribed),
			%%base:ilog(?MODULE, "subscribed~n",[]),
			switch:publish(?MODULE, ready, self());
		
		
		%% SAVE the assigned port to the CTL file
		{_From, management_port, Port} ->
			hevent({management_port, Port});
		
		
		{from_client, Msg} ->
			hevent({from_client, Msg}),
			ok;

		
		Other ->
			base:ilog(?MODULE, "received [~p]~n", [Other])
	
	end,
	loop().


hevent(E) ->
	State   = get(state),
	%%base:ilog(?MODULE, "hevent: State[~p] Event[~p]~n",[State, E]),	
	hcevent(State, E).
	

%% DAEMON STARTED
%% ==============

hcevent(_, {management_port, Port}) ->
	DoSave = fun(Porte) ->
					base:ilog(daemon_client, "saving port [~p] in CTL file~n", [Porte]),
					base:saveport(Porte),
					put(management_port, Porte)
			  end,
	base:condexec(diff_value, management_port, Port, DoSave, Port);


%% The management client is asking us to exit daemon state...
hcevent(_, {from_client, do_exit}) ->
	Pid = os:getpid(),
	switch:publish(daemon_ctl, daemon_exit, Pid);


%% The management client is inquiring about our PID...
hcevent(_, {from_client, what_pid}) ->
	Pid = os:getpid(),
	switch:publish(daemon_ctl, to_client, {pid, Pid});


%%%%CATCH-ALL%%%%
hcevent(State, Event) ->
	base:elog(?MODULE, "hcevent: INVALID PATTERN, Daemon: State[~p] Event[~p]~n", [State, Event]).



