%%%===================================================================
%% @doc
%%	测试表
%% @end
%%%===================================================================
-module(dbT_test).
-author(sql).
-include("dbT_test.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_id1/1, get_key1/1, get_var1/1, get_var2/1, get_var3/1, get_var4/1, get_var5/1, get_var6/1, get_var7/1, get_var8/1]).
-export([set_id1/2, set_key1/2, set_var1/2, set_var2/2, set_var3/2, set_var4/2, set_var5/2, set_var6/2, set_var7/2, set_var8/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #test{}.
init(Key1) when is_integer(Key1)->
	 #test{id1 = Key1}.

db_src() ->
	[game].

key_index() ->
	2.

check(Rec) ->
	is_float(Rec#test.var8) andalso is_float(Rec#test.var7) andalso is_float(Rec#test.var6) andalso is_tuple(Rec#test.var5) andalso is_list(Rec#test.var4) andalso is_binary(Rec#test.var3) andalso is_list(Rec#test.var2) andalso is_atom(Rec#test.var1) andalso is_integer(Rec#test.key1) andalso is_integer(Rec#test.id1).

get_id1(#test{id1 = Var}) -> Var.
get_key1(#test{key1 = Var}) -> Var.
get_var1(#test{var1 = Var}) -> Var.
get_var2(#test{var2 = Var}) -> Var.
get_var3(#test{var3 = Var}) -> Var.
get_var4(#test{var4 = Var}) -> Var.
get_var5(#test{var5 = Var}) -> Var.
get_var6(#test{var6 = Var}) -> Var.
get_var7(#test{var7 = Var}) -> Var.
get_var8(#test{var8 = Var}) -> Var.

set_id1(R, Var) when is_integer(Var) ->
	R#test{id1 = Var}.
set_key1(R, Var) when is_integer(Var) ->
	R#test{key1 = Var}.
set_var1(R, Var) when is_atom(Var) ->
	R#test{var1 = Var}.
set_var2(R, Var) when is_list(Var) ->
	R#test{var2 = Var}.
set_var3(R, Var) when is_binary(Var) ->
	R#test{var3 = Var}.
set_var4(R, Var) when is_list(Var) ->
	R#test{var4 = Var}.
set_var5(R, Var) when is_tuple(Var) ->
	R#test{var5 = Var}.
set_var6(R, Var) when is_float(Var) ->
	R#test{var6 = Var}.
set_var7(R, Var) when is_float(Var) ->
	R#test{var7 = Var}.
set_var8(R, Var) when is_float(Var) ->
	R#test{var8 = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`id1`">>, 'integer', 'INT', <<"id1">>};
info_by_index(3) ->
	{<<"`key1`">>, 'integer', 'BIGINT', <<"数字">>};
info_by_index(4) ->
	{<<"`var1`">>, 'atom', {'VARCHAR',200}, <<"var2">>};
info_by_index(5) ->
	{<<"`var2`">>, 'string', {'VARCHAR',200}, <<"var2">>};
info_by_index(6) ->
	{<<"`var3`">>, 'binary', {'VARCHAR',200}, <<"var3">>};
info_by_index(7) ->
	{<<"`var4`">>, 'list', {'VARCHAR',500}, <<"var4">>};
info_by_index(8) ->
	{<<"`var5`">>, 'tuple', {'VARCHAR',500}, <<"var5">>};
info_by_index(9) ->
	{<<"`var6`">>, 'float', 'FLOAT', <<"var6">>};
info_by_index(10) ->
	{<<"`var7`">>, 'float', 'FLOAT', <<"var7">>};
info_by_index(11) ->
	{<<"`var8`">>, 'float', 'FLOAT', <<"var8">>};
info_by_index(_) -> none.

index_by_var(<<"id1">>) -> 2;
index_by_var(<<"key1">>) -> 3;
index_by_var(<<"var1">>) -> 4;
index_by_var(<<"var2">>) -> 5;
index_by_var(<<"var3">>) -> 6;
index_by_var(<<"var4">>) -> 7;
index_by_var(<<"var5">>) -> 8;
index_by_var(<<"var6">>) -> 9;
index_by_var(<<"var7">>) -> 10;
index_by_var(<<"var8">>) -> 11;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `test` (
		`id1` INT DEFAULT '0' COMMENT 'id1',
		`key1` BIGINT DEFAULT '0' COMMENT '数字',
		`var1` VARCHAR(200) DEFAULT 'x' COMMENT 'var2',
		`var2` VARCHAR(200) DEFAULT '中文' COMMENT 'var2',
		`var3` VARCHAR(200) DEFAULT '' COMMENT 'var3',
		`var4` VARCHAR(500) DEFAULT '[x]' COMMENT 'var4',
		`var5` VARCHAR(500) DEFAULT '{}' COMMENT 'var5',
		`var6` FLOAT DEFAULT '0.0' COMMENT 'var6',
		`var7` FLOAT DEFAULT '0.0' COMMENT 'var7',
		`var8` FLOAT DEFAULT '0.0' COMMENT 'var8',
		PRIMARY KEY (`id1`),
		INDEX `key1`(`key1`)
	) ENGINE=Innodb DEFAULT CHARSET=utf8 COMMENT= '测试表';"/utf8>>.

table_name() ->
	<<"`test`">>.

primary_key() -> 
	<<"`id1`">>.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`test` WHERE `id1` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`test` WHERE `id1` = ?;">>, [DB])).

