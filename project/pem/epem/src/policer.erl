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
		 create_token_policer/1,
		 police/4
		 ]).

%% Local functions
-export([
		 loop/1,
		 do_policing/4
		 ]).

%%
%% API Functions
%%

%% Bucket = {id, Tokens, Period}
%% Buckets = [Bucket...]
%% `id` should be formatted as atom bucketXX where XX is unique
%% Period: interval duration in microseconds
create_token_policer(Buckets) ->
	Pid=spawn(?MODULE, loop, [Buckets]),
	Pid ! start,
	{ok, Pid}.


%% 
police(Policer, ReplyTo, PassMsg, DropMsg) ->
	Policer ! {police, ReplyTo, PassMsg, DropMsg}.


%%
%% Local Functions
%%

loop(Buckets) ->
	receive

		{police, ReplyTo, PassMsg, DropMsg} ->
			do_policing(Buckets, ReplyTo, PassMsg, DropMsg);

		
		stop ->
			exit(ok)
		
	end,
	loop(Buckets).




do_policing(Bucket, ReplyTo, PassMsg, DropMsg) when is_tuple(Bucket) ->
	
	{Id, MaxTokens, Period} = Bucket,
	CurrentTime=now(),
	
	%% retrieve start of interval
	StartPeriod=base:getvar({start_period, Id}, CurrentTime),
	
	%% current token count
	Tokens=base:getvar({tokens, Id}, 0),
	
	%% is interval finished?
	Diff=timer:now_diff(CurrentTime, StartPeriod),
	
	if 
		Diff > Period ->
			%% end of period... start a new one
			put({tokens, Id}, 0),
			put({start_period, Id}, CurrentTime),
			Tokens2=0,
			ok;
		
		true ->
			%% in the period, add a token to the bucket
			%%  and see if overflow occurs
			Tokens2=Tokens
		 
	end,
	
	NewCount = Tokens2 + 1,
	put({tokens, Id}, NewCount),
	
	if 
		NewCount > MaxTokens ->
			%% Drop
			ReplyTo ! DropMsg,
			drop;
		
		true ->
			ReplyTo ! PassMsg,
			pass
	end;




do_policing(Buckets, ReplyTo, PassMsg, DropMsg) when is_list(Buckets) ->
	[Head|Tail] = Buckets,
	do_policing(Head, ReplyTo, PassMsg, DropMsg),
	do_policing(Tail, ReplyTo, PassMsg, DropMsg).



