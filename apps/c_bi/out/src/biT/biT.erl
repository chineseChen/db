%%%===================================================================
%% @doc
%% biT 
%% @end
%%%===================================================================
-module(biT).
-author(sql).

-export([get_mod/0, get_md5/1, get_file/1]).

get_mod() ->
	{{test,biT_test}}.

get_md5(test) ->
	<<"f0ce62d5d648506669dbab38b51b2104">>;
get_md5(_) -> 
	 none.

get_file(test) ->
	"./apps/c_bi/out/src/test_bi/biT_test.erl";
get_file(_) ->
	none.

