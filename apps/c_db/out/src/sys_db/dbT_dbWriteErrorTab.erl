%%%===================================================================
%% @doc
%%	写出错的表
%% @end
%%%===================================================================
-module(dbT_dbWriteErrorTab).
-author(sql).
-include("dbT_dbWriteErrorTab.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_id/1, get_time/1, get_error/1, get_value/1]).
-export([set_id/2, set_time/2, set_error/2, set_value/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #dbWriteErrorTab{}.
init(Key1) when is_binary(Key1)->
	 #dbWriteErrorTab{id = Key1}.

db_src() ->
	[sys].

key_index() ->
	2.

check(Rec) ->
	is_binary(Rec#dbWriteErrorTab.value) andalso is_binary(Rec#dbWriteErrorTab.error) andalso is_integer(Rec#dbWriteErrorTab.time) andalso is_binary(Rec#dbWriteErrorTab.id).

get_id(#dbWriteErrorTab{id = Var}) -> Var.
get_time(#dbWriteErrorTab{time = Var}) -> Var.
get_error(#dbWriteErrorTab{error = Var}) -> Var.
get_value(#dbWriteErrorTab{value = Var}) -> Var.

set_id(R, Var) when is_binary(Var) ->
	R#dbWriteErrorTab{id = Var}.
set_time(R, Var) when is_integer(Var) ->
	R#dbWriteErrorTab{time = Var}.
set_error(R, Var) when is_binary(Var) ->
	R#dbWriteErrorTab{error = Var}.
set_value(R, Var) when is_binary(Var) ->
	R#dbWriteErrorTab{value = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`id`">>, 'binary', {'VARCHAR',200}, <<"表名">>};
info_by_index(3) ->
	{<<"`time`">>, 'integer', 'INT', <<"最近一次变化时间,单位s">>};
info_by_index(4) ->
	{<<"`error`">>, 'binary', 'TEXT', <<"错误原因">>};
info_by_index(5) ->
	{<<"`value`">>, 'binary', 'TEXT', <<"内容">>};
info_by_index(_) -> none.

index_by_var(<<"id">>) -> 2;
index_by_var(<<"time">>) -> 3;
index_by_var(<<"error">>) -> 4;
index_by_var(<<"value">>) -> 5;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `dbWriteErrorTab` (
		`id` VARCHAR(200) DEFAULT '' COMMENT '表名',
		`time` INT DEFAULT '0' COMMENT '最近一次变化时间,单位s',
		`error` TEXT COMMENT '错误原因',
		`value` TEXT COMMENT '内容',
		PRIMARY KEY (`id`)
	) ENGINE=Innodb DEFAULT CHARSET=utf8 COMMENT= '写出错的表';"/utf8>>.

table_name() ->
	<<"`dbWriteErrorTab`">>.

primary_key() -> 
	<<"`id`">>.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`dbWriteErrorTab` WHERE `id` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`dbWriteErrorTab` WHERE `id` = ?;">>, [DB])).

