%% Author: Jean-Lou Dupont
%% Created: 2009-07-24
%% Description: TODO: Add description to policer
-module(policer).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 create_dual_token/4,
		 police/4
		 ]).

%% Local functions
-export([
		 loop/1,
		 do_policing/3
		 ]).

%%
%% API Functions
%%

%% Dual Token Bucket
create_dual_token(Tokens1, Time1, Tokens2, Time2) ->
	Bucket={Tokens1, Time1, Tokens2, Time2},
	Pid=spawn(?MODULE, loop, [Bucket]),
	Pid ! start,
	{ok, Pid}.


police(Policer, Msg, PassMsg, DropMsg) ->
	Policer ! {police, Msg, PassMsg, DropMsg}.


%%
%% Local Functions
%%

loop(Bucket) ->
	receive

		{police, Msg, PassMsg, DropMsg} ->
			do_policing(Msg, PassMsg, DropMsg);

		
		stop ->
			exit(ok)
		
	end,
	loop(Bucket).


do_policing(Msg, PassMsg, DropMsg) ->
	ok.