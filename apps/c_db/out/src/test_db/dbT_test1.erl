%%%===================================================================
%% @doc
%%	测试表
%% @end
%%%===================================================================
-module(dbT_test1).
-author(sql).
-include("dbT_test1.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_id1/1, get_key1/1]).
-export([set_id1/2, set_key1/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #test1{}.
init(Key1) when is_integer(Key1)->
	 #test1{id1 = Key1}.

db_src() ->
	[game].

key_index() ->
	2.

check(Rec) ->
	is_binary(Rec#test1.key1) andalso is_integer(Rec#test1.id1).

get_id1(#test1{id1 = Var}) -> Var.
get_key1(#test1{key1 = Var}) -> Var.

set_id1(R, Var) when is_integer(Var) ->
	R#test1{id1 = Var}.
set_key1(R, Var) when is_binary(Var) ->
	R#test1{key1 = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`id1`">>, 'integer', 'INT', <<"id1">>};
info_by_index(3) ->
	{<<"`key1`">>, 'binary', 'TEXT', <<"测试字符串">>};
info_by_index(_) -> none.

index_by_var(<<"id1">>) -> 2;
index_by_var(<<"key1">>) -> 3;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `test1` (
		`id1` INT NOT NULL COMMENT 'id1' AUTO_INCREMENT,
		`key1` TEXT COMMENT '测试字符串',
		PRIMARY KEY (`id1`)
	) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT= '测试表' AUTO_INCREMENT=1;"/utf8>>.

table_name() ->
	<<"`test1`">>.

primary_key() -> 
	<<"`id1`">>.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`test1` WHERE `id1` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`test1` WHERE `id1` = ?;">>, [DB])).

