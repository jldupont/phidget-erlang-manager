%%% -------------------------------------------------------------------
%%% Author  : Jean-Lou Dupont
%%% Description :
%%%
%%% Created : 2009-06-22
%%% -------------------------------------------------------------------
-module(pem_admin_sup).

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	 init/1,
	 start_link/0
        ]).

%% ====================================================================
%% Server functions
%% ====================================================================
start_link() ->
	process_flag(trap_exit,true),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% --------------------------------------------------------------------
%% Func: init/0
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
    Child_reflector = {reflector,{reflector,start_link,[]},
	      permanent,2000,worker,[reflector]},

    Child_client = {daemon_client,{daemon_client,start_link,[]},
	      permanent,2000,worker,[daemon_client]},
	
	
    {ok,{{one_for_one,5,1}, [Child_reflector,
							 Child_client
							 ]}}.
