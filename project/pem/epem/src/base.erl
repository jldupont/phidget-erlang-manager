%% Author: Jean-Lou Dupont
%% Created: 2009-06-19
%% Description: TODO: Add description to base
-module(base).

%%
%% Include files
%%
-include_lib("kernel/include/file.hrl").

%%
%% Exported Functions
%%
-export([
		 home/0,
		 home/1,
		 elog/2,
		 ilog/2,
		 elog/3,
		 ilog/3,
		 is_debug/1,
		 safe_mkdir/1,
		 safe_mkdir/2,
		 is_dir/1,
		 is_file/1,
		 path_type/1
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

home(Env_var) ->
    case os:getenv(Env_var) of
		false ->
		    os:getenv("HOME");
		DIR ->
	    	DIR
    end.
	


safe_mkdir(Dir) ->
	Type = path_type(Dir),
	safe_mkdir(Dir, Type).

%% Path already exists and its a directory => nothing todo
safe_mkdir(_Dir, {ok, directory}) ->
	ok;

%% Path already exists and its NOT a directory => error
safe_mkdir(_Dir, {ok, Type}) ->
	{error, Type};
	

%% Path does not exist... create as directory then
safe_mkdir(Dir, {error, _}) ->
	file:make_dir(Dir).


is_file(Path) ->
	case path_type(Path) of
		{ok, regular} ->
			true;
		_ ->
			false
	end.



is_dir(Path) ->
	case path_type(Path) of
		{ok, directory} ->
			true;
		_ ->
			false
	end.


path_type(Path) ->
	{X, Y} = file:read_file_info(Path),
	case X of
		ok ->
			{ok, Y#file_info.type};
		_ ->
			{X, Y}
	end.


