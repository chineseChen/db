%%%===================================================================
%% @doc
%%	
%% @end
%%%===================================================================
-module(dbT_session).
-author(sql).
-include("dbT_session.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_userId/1, get_sign/1]).
-export([set_userId/2, set_sign/2]).
-export([save_type/0]).

init() ->
	 #session{}.
init(Key1) when is_integer(Key1)->
	 #session{userId = Key1}.

db_src() ->
	[game].

key_index() ->
	2.

check(Rec) ->
	is_binary(Rec#session.sign) andalso is_integer(Rec#session.userId).

get_userId(#session{userId = Var}) -> Var.
get_sign(#session{sign = Var}) -> Var.

set_userId(R, Var) when is_integer(Var) ->
	R#session{userId = Var}.
set_sign(R, Var) when is_binary(Var) ->
	R#session{sign = Var}.

save_type() ->
	memory.

