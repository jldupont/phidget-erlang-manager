%% Author: Jean-Lou Dupont
%% Created: 2009-06-19
%% Description: TODO: Add description to base
-module(base).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 home/0,
		 elog/2,
		 ilog/2,
		 elog/3,
		 ilog/3,
		 is_debug/1
		 ]).

%%
%% API Functions
%%

elog(X,Y) ->
	error_logger:error_msg("~p: "++X, [?MODULE|Y]).

ilog(X,Y) ->
	error_logger:info_msg("~p: "++X, [?MODULE|Y]).

elog(M, X,Y) ->
	error_logger:error_msg("~p: "++X, [M|Y]).

ilog(M, X,Y) ->
	error_logger:info_msg("~p: "++X, [M|Y]).

%% Verifies if an ATOM(debug) is present in Args
%% The paramter Args can either be a single ATOM
%% or a list
is_debug(Args) ->
	case is_list(Args) of
		true ->
			Dbg = Args--[debug],
			case Dbg of
				debug ->
					Debug = true;
				_ ->
					Debug = false
			end;
		false ->
			case Args of
				debug ->
					Debug = true;
				_ ->
					Debug = false
			end
	end,
	Debug.


home() ->
    case os:getenv("PEMHOME") of
		false ->
		    os:getenv("HOME");
		DIR ->
	    	DIR
    end.
