%%% -------------------------------------------------------------------
%%% Author  : Jean-Lou Dupont
%%% Description :
%%%
%%% Created : 2009-06-22
%%% -------------------------------------------------------------------
-module(pem_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1,
	 start_link/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).


%% ====================================================================
%% Server functions
%% ====================================================================
start_link(Args) ->
	process_flag(trap_exit,true),
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(Args) ->
    Child_reflector = {reflector,{reflector,start_link,Args},
	      permanent,2000,worker,[reflector]},

    Child_journal = {journal,{journal,start_link,Args},
	      permanent,2000,worker,[journal]},
	
    Child_manager = {manager,{manager,start_link,Args},
	      permanent,2000,worker,[manager]},

    Child_ifk = {ifk,{ifk,start_link,Args},
	      permanent,2000,worker,[ifk]},
	
    {ok,{{one_for_one,5,1}, [Child_reflector,
							 Child_journal,
							 Child_manager, 
							 Child_ifk]}}.
