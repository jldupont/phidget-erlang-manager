%% Author: Jean-Lou Dupont
%% Created: 2009-07-28
%% Description: 
-module(appmod_events).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 out/1
		 ]).

%%
%% API Functions
%%
out(Args) ->
	timer:sleep(5*1000),
	{html, io_lib:format("Args: ~p",[Args])}.


%%
%% Local Functions
%%

