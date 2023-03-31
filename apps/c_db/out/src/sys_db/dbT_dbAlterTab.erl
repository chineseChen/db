%%%===================================================================
%% @doc
%%	表alter信息
%% @end
%%%===================================================================
-module(dbT_dbAlterTab).
-author(sql).
-include("dbT_dbAlterTab.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_tabName/1, get_time/1, get_version/1]).
-export([set_tabName/2, set_time/2, set_version/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #dbAlterTab{}.
init(Key1) when is_binary(Key1)->
	 #dbAlterTab{tabName = Key1}.

db_src() ->
	[sys].

key_index() ->
	2.

check(Rec) ->
	is_tuple(Rec#dbAlterTab.version) andalso is_integer(Rec#dbAlterTab.time) andalso is_binary(Rec#dbAlterTab.tabName).

get_tabName(#dbAlterTab{tabName = Var}) -> Var.
get_time(#dbAlterTab{time = Var}) -> Var.
get_version(#dbAlterTab{version = Var}) -> Var.

set_tabName(R, Var) when is_binary(Var) ->
	R#dbAlterTab{tabName = Var}.
set_time(R, Var) when is_integer(Var) ->
	R#dbAlterTab{time = Var}.
set_version(R, Var) when is_tuple(Var) ->
	R#dbAlterTab{version = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`tabName`">>, 'binary', {'VARCHAR',200}, <<"表名">>};
info_by_index(3) ->
	{<<"`time`">>, 'integer', 'INT', <<"最近一次变化时间,单位s">>};
info_by_index(4) ->
	{<<"`version`">>, 'tuple', {'VARCHAR',200}, <<"版本号">>};
info_by_index(_) -> none.

index_by_var(<<"tabName">>) -> 2;
index_by_var(<<"time">>) -> 3;
index_by_var(<<"version">>) -> 4;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `dbAlterTab` (
		`tabName` VARCHAR(200) DEFAULT '' COMMENT '表名',
		`time` INT DEFAULT '0' COMMENT '最近一次变化时间,单位s',
		`version` VARCHAR(200) DEFAULT '{0,0,0}' COMMENT '版本号',
		PRIMARY KEY (`tabName`)
	) ENGINE=Innodb DEFAULT CHARSET=utf8 COMMENT= '表alter信息';"/utf8>>.

table_name() ->
	<<"`dbAlterTab`">>.

primary_key() -> 
	<<"`tabName`">>.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`dbAlterTab` WHERE `tabName` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`dbAlterTab` WHERE `tabName` = ?;">>, [DB])).

