%%%===================================================================
%% @doc
%% dbT 
%% @end
%%%===================================================================
-module(dbT).
-author(sql).

-export([get_mod/0, get_md5/1, get_file/1]).

get_mod() ->
	{{dbAlterTab,dbT_dbAlterTab},
 {dbWriteErrorTab,dbT_dbWriteErrorTab},
 {keyValue,dbT_keyValue},
 {test,dbT_test},
 {test1,dbT_test1},
 {test2,[game]},
 {test3,dbT_test3},
 {test4,dbT_test4},
 {uid,dbT_uid}}.

get_md5(keyValue) ->
	<<"f521c09678492dae1d523600872dc872">>;
get_md5(test) ->
	<<"84905a6991ec9fcc66d14616676f376b">>;
get_md5(test1) ->
	<<"dae20724530820034a596d4cfa9d18ca">>;
get_md5(test3) ->
	<<"bec97a85463541819d4e5b222946d9cd">>;
get_md5(test4) ->
	<<"c00628ab73dd661a21d844e24cbe31a7">>;
get_md5(uid) ->
	<<"966fb6b9880973269199a82c15458386">>;
get_md5(dbAlterTab) ->
	<<"776fe55a47c17acb5e1728e91230ad35">>;
get_md5(dbWriteErrorTab) ->
	<<"6dfbffd98be08673dfbafc74a0a8afda">>;
get_md5(_) -> 
	 none.

get_file(keyValue) ->
	"./apps/c_db/out/src/kv_db/dbT_keyValue.erl";
get_file(test) ->
	"./apps/c_db/out/src/test_db/dbT_test.erl";
get_file(test1) ->
	"./apps/c_db/out/src/test_db/dbT_test1.erl";
get_file(test3) ->
	"./apps/c_db/out/src/test_db/dbT_test3.erl";
get_file(test4) ->
	"./apps/c_db/out/src/test_db/dbT_test4.erl";
get_file(uid) ->
	"./apps/c_db/out/src/uid_db/dbT_uid.erl";
get_file(dbAlterTab) ->
	"./apps/c_db/out/src/sys_db/dbT_dbAlterTab.erl";
get_file(dbWriteErrorTab) ->
	"./apps/c_db/out/src/sys_db/dbT_dbWriteErrorTab.erl";
get_file(_) ->
	none.

