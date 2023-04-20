%%%===================================================================
%% @doc
%%	帐号
%% @end
%%%===================================================================
-module(dbT_account).
-author(sql).
-include("dbT_account.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_id/1, get_userUid/1]).
-export([set_id/2, set_userUid/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #account{}.
init(Key1) when is_tuple(Key1)->
	 #account{id = Key1}.

db_src() ->
	[login].

key_index() ->
	2.

check(Rec) ->
	is_integer(Rec#account.userUid) andalso is_tuple(Rec#account.id).

get_id(#account{id = Var}) -> Var.
get_userUid(#account{userUid = Var}) -> Var.

set_id(R, Var) when is_tuple(Var) ->
	R#account{id = Var}.
set_userUid(R, Var) when is_integer(Var) ->
	R#account{userUid = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`id`">>, 'tuple', {'VARCHAR',200}, <<"玩家帐号">>};
info_by_index(3) ->
	{<<"`userUid`">>, 'integer', 'BIGINT', <<"角色uid">>};
info_by_index(_) -> none.

index_by_var(<<"id">>) -> 2;
index_by_var(<<"userUid">>) -> 3;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `account` (
		`id` VARCHAR(200) DEFAULT '{0,0,0}' COMMENT '玩家帐号',
		`userUid` BIGINT DEFAULT '0' COMMENT '角色uid',
		PRIMARY KEY (`id`),
		INDEX `userUid`(`userUid`)
	) ENGINE=Innodb DEFAULT CHARSET=utf8 COMMENT= '帐号';"/utf8>>.

table_name() ->
	<<"`account`">>.

primary_key() -> 
	<<"`id`">>.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`account` WHERE `id` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`account` WHERE `id` = ?;">>, [DB])).

