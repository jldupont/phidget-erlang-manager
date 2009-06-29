%% Author: Jean-Lou Dupont
%% Created: 2009-06-29
%% Description: TODO: Add description to rpc
-module(rpc).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 call/1

		 ]).

%%
%% API Functions
%%


call({Cmd, Cb, Args}) ->
	Spid=self(),
	Ret = switch ! {Spid, Cmd, Cb, Args},
	case Ret of
		{Spid, Cmd, Cb, Args} ->
			ok;
		_ ->
			error
	end.



%%
%% Local Functions
%%

