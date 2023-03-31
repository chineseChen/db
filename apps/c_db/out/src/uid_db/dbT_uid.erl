%%%===================================================================
%% @doc
%%	uid生成器
%% @end
%%%===================================================================
-module(dbT_uid).
-author(sql).
-include("dbT_uid.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_type/1, get_index/1]).
-export([set_type/2, set_index/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #uid{}.
init(Key1) when is_atom(Key1)->
	 #uid{type = Key1}.

db_src() ->
	[local].

key_index() ->
	2.

check(Rec) ->
	is_integer(Rec#uid.index) andalso is_atom(Rec#uid.type).

get_type(#uid{type = Var}) -> Var.
get_index(#uid{index = Var}) -> Var.

set_type(R, Var) when is_atom(Var) ->
	R#uid{type = Var}.
set_index(R, Var) when is_integer(Var) ->
	R#uid{index = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`type`">>, 'atom', {'VARCHAR',50}, <<"类型">>};
info_by_index(3) ->
	{<<"`index`">>, 'integer', 'BIGINT', <<"生成uid系数">>};
info_by_index(_) -> none.

index_by_var(<<"type">>) -> 2;
index_by_var(<<"index">>) -> 3;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `uid` (
		`type` VARCHAR(50) DEFAULT 'common' COMMENT '类型',
		`index` BIGINT DEFAULT '0' COMMENT '生成uid系数',
		PRIMARY KEY (`type`)
	) ENGINE=Innodb DEFAULT CHARSET=utf8 COMMENT= 'uid生成器';"/utf8>>.

table_name() ->
	<<"`uid`">>.

primary_key() -> 
	<<"`type`">>.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`uid` WHERE `type` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`uid` WHERE `type` = ?;">>, [DB])).

