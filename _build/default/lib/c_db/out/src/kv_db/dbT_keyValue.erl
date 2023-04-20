%%%===================================================================
%% @doc
%%	键值对
%% @end
%%%===================================================================
-module(dbT_keyValue).
-author(sql).
-include("dbT_keyValue.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_key/1, get_value/1]).
-export([set_key/2, set_value/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #keyValue{}.
init(Key1) when is_atom(Key1)->
	 #keyValue{key = Key1}.

db_src() ->
	[local].

key_index() ->
	2.

check(Rec) ->
	is_atom(Rec#keyValue.key).

get_key(#keyValue{key = Var}) -> Var.
get_value(#keyValue{value = Var}) -> Var.

set_key(R, Var) when is_atom(Var) ->
	R#keyValue{key = Var}.
set_value(R, Var)->
	R#keyValue{value = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`key`">>, 'atom', {'VARCHAR',200}, <<"键">>};
info_by_index(3) ->
	{<<"`value`">>, 'term', 'TEXT', <<"值">>};
info_by_index(_) -> none.

index_by_var(<<"key">>) -> 2;
index_by_var(<<"value">>) -> 3;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `keyValue` (
		`key` VARCHAR(200) DEFAULT 'x' COMMENT '键',
		`value` TEXT COMMENT '值',
		PRIMARY KEY (`key`)
	) ENGINE=Innodb DEFAULT CHARSET=utf8 COMMENT= '键值对';"/utf8>>.

table_name() ->
	<<"`keyValue`">>.

primary_key() -> 
	<<"`key`">>.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`keyValue` WHERE `key` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`keyValue` WHERE `key` = ?;">>, [DB])).

