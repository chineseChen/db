%%%===================================================================
%% @doc
%%	
%% @end
%%%===================================================================
-module(dbT_test3).
-author(sql).
-include("dbT_test3.hrl").

-export([init/0, init/2, db_src/0, key_index/0, check/1]).
-export([get_id1/1, get_key1/1, get_var/1]).
-export([set_id1/2, set_key1/2, set_var/2]).
-export([save_type/0]).

init() ->
	 #test3{}.
init(Key1, Key2) when is_integer(Key1) andalso is_integer(Key2)->
	 #test3{id1 = Key1,key1 = Key2}.

db_src() ->
	[game].

key_index() ->
	{2,3}.

check(Rec) ->
	is_integer(Rec#test3.var) andalso is_integer(Rec#test3.key1) andalso is_integer(Rec#test3.id1).

get_id1(#test3{id1 = Var}) -> Var.
get_key1(#test3{key1 = Var}) -> Var.
get_var(#test3{var = Var}) -> Var.

set_id1(R, Var) when is_integer(Var) ->
	R#test3{id1 = Var}.
set_key1(R, Var) when is_integer(Var) ->
	R#test3{key1 = Var}.
set_var(R, Var) when is_integer(Var) ->
	R#test3{var = Var}.

save_type() ->
	memory.

