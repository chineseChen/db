%%%===================================================================
%% @doc
%%	测试表多键
%% @end
%%%===================================================================
-module(dbT_test4).
-author(sql).
-include("dbT_test4.hrl").

-export([init/0, init/2, db_src/0, key_index/0, check/1]).
-export([get_id1/1, get_key1/1, get_key2/1]).
-export([set_id1/2, set_key1/2, set_key2/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #test4{}.
init(Key1, Key2) when is_integer(Key1) andalso is_integer(Key2)->
	 #test4{id1 = Key1,key1 = Key2}.

db_src() ->
	[game].

key_index() ->
	{2,3}.

check(Rec) ->
	is_integer(Rec#test4.key2) andalso is_integer(Rec#test4.key1) andalso is_integer(Rec#test4.id1).

get_id1(#test4{id1 = Var}) -> Var.
get_key1(#test4{key1 = Var}) -> Var.
get_key2(#test4{key2 = Var}) -> Var.

set_id1(R, Var) when is_integer(Var) ->
	R#test4{id1 = Var}.
set_key1(R, Var) when is_integer(Var) ->
	R#test4{key1 = Var}.
set_key2(R, Var) when is_integer(Var) ->
	R#test4{key2 = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`id1`">>, 'integer', 'INT', <<"id1">>};
info_by_index(3) ->
	{<<"`key1`">>, 'integer', 'BIGINT', <<"key1">>};
info_by_index(4) ->
	{<<"`key2`">>, 'integer', 'BIGINT', <<"key2">>};
info_by_index(_) -> none.

index_by_var(<<"id1">>) -> 2;
index_by_var(<<"key1">>) -> 3;
index_by_var(<<"key2">>) -> 4;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `test4` (
		`id1` INT DEFAULT '0' COMMENT 'id1',
		`key1` BIGINT DEFAULT '0' COMMENT 'key1',
		`key2` BIGINT DEFAULT '0' COMMENT 'key2',
		PRIMARY KEY (`id1`, `key1`)
	) ENGINE=Innodb DEFAULT CHARSET=utf8 COMMENT= '测试表多键';"/utf8>>.

table_name() ->
	<<"`test4`">>.

primary_key() -> 
	{<<"`id1`">>,<<"`key1`">>}.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`test4` WHERE `id1` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`test4` WHERE `id1` = ?;">>, [DB])).

