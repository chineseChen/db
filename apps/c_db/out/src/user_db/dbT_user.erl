%%%===================================================================
%% @doc
%%	用户
%% @end
%%%===================================================================
-module(dbT_user).
-author(sql).
-include("dbT_user.hrl").

-export([init/0, init/1, db_src/0, key_index/0, check/1]).
-export([get_userUid/1, get_sign/1, get_accountId/1, get_passWord/1, get_pId/1, get_channel/1, get_time/1, get_ip/1, get_clientOs/1, get_deviceId/1]).
-export([set_userUid/2, set_sign/2, set_accountId/2, set_passWord/2, set_pId/2, set_channel/2, set_time/2, set_ip/2, set_clientOs/2, set_deviceId/2]).
-export([save_type/0]).
-export([info_by_index/1, index_by_var/1]).
-export([table_name/0, primary_key/0, create/0, select/1, delete/1]).

init() ->
	 #user{}.
init(Key1) when is_integer(Key1)->
	 #user{userUid = Key1}.

db_src() ->
	[login].

key_index() ->
	2.

check(Rec) ->
	is_binary(Rec#user.deviceId) andalso is_atom(Rec#user.clientOs) andalso is_binary(Rec#user.ip) andalso is_integer(Rec#user.time) andalso is_integer(Rec#user.channel) andalso is_integer(Rec#user.pId) andalso is_binary(Rec#user.passWord) andalso is_binary(Rec#user.accountId) andalso is_binary(Rec#user.sign) andalso is_integer(Rec#user.userUid).

get_userUid(#user{userUid = Var}) -> Var.
get_sign(#user{sign = Var}) -> Var.
get_accountId(#user{accountId = Var}) -> Var.
get_passWord(#user{passWord = Var}) -> Var.
get_pId(#user{pId = Var}) -> Var.
get_channel(#user{channel = Var}) -> Var.
get_time(#user{time = Var}) -> Var.
get_ip(#user{ip = Var}) -> Var.
get_clientOs(#user{clientOs = Var}) -> Var.
get_deviceId(#user{deviceId = Var}) -> Var.

set_userUid(R, Var) when is_integer(Var) ->
	R#user{userUid = Var}.
set_sign(R, Var) when is_binary(Var) ->
	R#user{sign = Var}.
set_accountId(R, Var) when is_binary(Var) ->
	R#user{accountId = Var}.
set_passWord(R, Var) when is_binary(Var) ->
	R#user{passWord = Var}.
set_pId(R, Var) when is_integer(Var) ->
	R#user{pId = Var}.
set_channel(R, Var) when is_integer(Var) ->
	R#user{channel = Var}.
set_time(R, Var) when is_integer(Var) ->
	R#user{time = Var}.
set_ip(R, Var) when is_binary(Var) ->
	R#user{ip = Var}.
set_clientOs(R, Var) when is_atom(Var) ->
	R#user{clientOs = Var}.
set_deviceId(R, Var) when is_binary(Var) ->
	R#user{deviceId = Var}.

save_type() ->
	disk.

info_by_index(2) ->
	{<<"`userUid`">>, 'integer', 'BIGINT', <<"角色uid">>};
info_by_index(3) ->
	{<<"`sign`">>, 'binary', {'VARCHAR',200}, <<"签名">>};
info_by_index(4) ->
	{<<"`accountId`">>, 'binary', {'VARCHAR',200}, <<"玩家帐号">>};
info_by_index(5) ->
	{<<"`passWord`">>, 'binary', {'VARCHAR',200}, <<"帐号密码">>};
info_by_index(6) ->
	{<<"`pId`">>, 'integer', 'INT', <<"平台">>};
info_by_index(7) ->
	{<<"`channel`">>, 'integer', 'INT', <<"渠道">>};
info_by_index(8) ->
	{<<"`time`">>, 'integer', 'INT', <<"最近一次登陆时间">>};
info_by_index(9) ->
	{<<"`ip`">>, 'binary', {'VARCHAR',100}, <<"最近一次登陆ip">>};
info_by_index(10) ->
	{<<"`clientOs`">>, 'atom', {'VARCHAR',50}, <<"设备操作系统">>};
info_by_index(11) ->
	{<<"`deviceId`">>, 'binary', {'VARCHAR',100}, <<"最近一次登陆设备id">>};
info_by_index(_) -> none.

index_by_var(<<"userUid">>) -> 2;
index_by_var(<<"sign">>) -> 3;
index_by_var(<<"accountId">>) -> 4;
index_by_var(<<"passWord">>) -> 5;
index_by_var(<<"pId">>) -> 6;
index_by_var(<<"channel">>) -> 7;
index_by_var(<<"time">>) -> 8;
index_by_var(<<"ip">>) -> 9;
index_by_var(<<"clientOs">>) -> 10;
index_by_var(<<"deviceId">>) -> 11;
index_by_var(_) -> none.

create() ->
	<<"CREATE TABLE  IF NOT EXISTS `user` (
		`userUid` BIGINT DEFAULT '0' COMMENT '角色uid',
		`sign` VARCHAR(200) DEFAULT '' COMMENT '签名',
		`accountId` VARCHAR(200) DEFAULT '' COMMENT '玩家帐号',
		`passWord` VARCHAR(200) DEFAULT '' COMMENT '帐号密码',
		`pId` INT DEFAULT '0' COMMENT '平台',
		`channel` INT DEFAULT '0' COMMENT '渠道',
		`time` INT DEFAULT '0' COMMENT '最近一次登陆时间',
		`ip` VARCHAR(100) DEFAULT '' COMMENT '最近一次登陆ip',
		`clientOs` VARCHAR(50) DEFAULT 'none' COMMENT '设备操作系统',
		`deviceId` VARCHAR(100) DEFAULT '' COMMENT '最近一次登陆设备id',
		PRIMARY KEY (`userUid`),
		INDEX `accountId`(`accountId`)
	) ENGINE=Innodb DEFAULT CHARSET=utf8 COMMENT= '用户';"/utf8>>.

table_name() ->
	<<"`user`">>.

primary_key() -> 
	<<"`userUid`">>.

select(DB) ->
	list_to_binary(io_lib:format(<<"SELECT * FROM ~s.`user` WHERE `userUid` = ?;">>, [DB])).

delete(DB) ->
	list_to_binary(io_lib:format(<<"DELETE FROM ~s.`user` WHERE `userUid` = ?;">>, [DB])).

