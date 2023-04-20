%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%% 一次性写入,cache退出时持久化
%%% @end
%%% Created : 09. 1月 2023 0:50
%%%-------------------------------------------------------------------
-module(write1Sql).
-author("ckc").
-include("db_channel.hrl").
-record(runArg, {
    key = none :: any(),%%key
    count = 0 :: integer(), %%数量
    loopNum = 0 :: integer(), %%循环数量
    mod = none :: atom(), %%mod
    keyTab = none :: none, %%key表
    changeInfoTab = none :: reference(),%%变化信息表
    cacheTab = none :: reference(),%%缓存表
    handle = none :: atom(), %%操作类型
    res = [] :: list() %%结果表
}).

%% API
-export([terminateWrite/6]).

%%%===================================================================
%% @doc
%% 退出时写入
%% @end
%%%===================================================================
terminateWrite(SupTag, Mod, KeyTab, CacheTab, CacheInfoTab, ChangeInfoTab) ->
    ForNum = ets:info(ChangeInfoTab, 'size'), true = is_integer(ForNum),
    PidT = list_to_tuple([Pid || {{write, _}, Pid, _, _} <- supervisor:which_children('db_sql_sup')]),
    Size = size(PidT), {DBSrc, _} = SupTag,
    {ok, DBName0} = gen_server:call('db_sql_mgr', {'get_db_name', DBSrc}),
    DBName = atom_to_binary(DBName0),
    {Count, LoopNum} = application:get_env(c_db, mysqlBatchNum, {100, 10000}),
    rollBack(Mod, KeyTab, CacheTab, CacheInfoTab),
    RunArg = #runArg{keyTab = KeyTab, changeInfoTab = ChangeInfoTab, mod = Mod, cacheTab = CacheTab},
    F = fun
        (_I, '$end_of_table') -> {break, ok};
        (I, Key) ->
            RunArg1 = RunArg#runArg{key = Key, count = Count, loopNum = LoopNum},
            #runArg{handle = Handle, res = KeyL} = RunArg2 = loopSelect(RunArg1),
            [begin
                Index = c_lib:get_loop_index(I, Size),
                MysqlPid = element(Index, PidT),
                case Handle of
                    'delete' ->
                        Sql = writeSql:formDeleteSql(Mod, DBName, KeyL),
                        Result = gen_server:call(MysqlPid, {'query_by_sql', {Sql, 30000}}, infinity),
                        [write1Data(SupTag, MysqlPid, Mod, RunArg2) || Result =/= ok];
                    'replace' ->
                        Sql = writeSql:formReplace(Mod, DBName, CacheTab, KeyL),
                        [begin
                            Result = gen_server:call(MysqlPid, {'query_by_sql', {Sql, 30000}}, infinity),
                            [write1Data(SupTag, MysqlPid, Mod, RunArg2) || Result =/= ok]
                        end || is_binary(Sql)]
                end
            end || KeyL =/= []],
            ets:first(ChangeInfoTab)
    end,
    c_lib:for(F, ets:first(ChangeInfoTab), 1, ForNum + 1).

%%%===================================================================
%% @doc
%% 回滚所有
%% @end
%%%===================================================================
rollBack(Mod, KeyTab, CacheTab, CacheInfoTab) ->
    F = fun(Obj, _) ->
        case Obj of
            #cacheInfo{key = Key, running = #running{data = Value}} ->
                case Value =:= none orelse Value =:= [] of
                    true ->
                        ets:delete(KeyTab, Key),
                        ets:delete(CacheTab, Key);
                    false ->
                        ets:insert(KeyTab, {Key}),
                        [ets:delete(CacheTab, Key) || db_data:isMulKey(Mod)],
                        ets:insert(CacheTab, Value)
                end;
            _ -> ok
        end
    end,
    ets:foldl(F, [], CacheInfoTab),
    ok.

%%%===================================================================
%% @doc
%% 选择一类数据
%% @end
%%%===================================================================
loopSelect(#runArg{key = '$end_of_table'} = RunArg) -> RunArg;
loopSelect(#runArg{count = Count} = RunArg) when Count =< 0 -> RunArg;
loopSelect(#runArg{loopNum = LoopNum} = RunArg) when LoopNum =< 0 -> RunArg;
loopSelect(RunArg) ->
    RunArg1 = case selectData(RunArg) of
        {ok, Handle, NextKey} ->
            RunArg#runArg{
                key = NextKey,
                handle = Handle,
                count = RunArg#runArg.count - 1,
                res = [{RunArg#runArg.key, none} | RunArg#runArg.res]
            };
        {'$continue', NextKey} -> RunArg#runArg{key = NextKey}
    end,
    loopSelect(RunArg1#runArg{loopNum = RunArg1#runArg.loopNum - 1}).

%%%%===================================================================
%% @doc
%% 选择数据
%% @end
%%%===================================================================
selectData(RunArg) ->
    #runArg{key = Key, mod = Mod, keyTab = KeyTab, cacheTab = CacheTab, changeInfoTab = ChangeInfoTab, handle = Handle} = RunArg,
    NowHandle = writeSql:getHandle(Mod, KeyTab, CacheTab, Key),
    case Handle =:= none orelse NowHandle =:= Handle of
        true ->
            NextKey = ets:next(ChangeInfoTab, Key),
            ets:delete(ChangeInfoTab, Key),
            {ok, NowHandle, NextKey};
        false ->
            {'$continue', ets:next(ChangeInfoTab, Key)}
    end.

%%%===================================================================
%% @doc
%% 一个个插入
%% @end
%%%===================================================================
write1Data(SupTag, MysqlPid, Mod, RunArg) ->
    {DBSrc, _} = SupTag,
    {ok, DBName0} = gen_server:call('db_sql_mgr', {'get_db_name', DBSrc}),
    DBName = atom_to_binary(DBName0),
    F = case RunArg#runArg.handle of
        'delete' -> fun(Value) -> writeSql:formDeleteSql(Mod, DBName, [Value]) end;
        'change' -> fun(Value) -> writeSql:formReplace(Mod, DBName, RunArg#runArg.cacheTab, [Value]) end
    end,
    write1Sql(SupTag, MysqlPid, F, RunArg#runArg.res).

%%%===================================================================
%% @doc
%% 出错后复写
%% @end
%%%===================================================================
write1Sql(_Tag, _MysqlPid, _ComF, []) -> ok;
write1Sql(Tag, MysqlPid, ComF, [H | T]) ->
    Sql = ComF(H),
    [begin
        Result = gen_server:call(MysqlPid, {'query_by_sql', {Sql, 10000}}, infinity),
        [begin
            writeError(MysqlPid, Tag, element(1, H), Result),
            logger:error("mod:~p line:~p error:~p~n~p", [?MODULE, ?LINE, Result, Sql])
        end || Result =/= ok]
    end || is_binary(Sql)],
    write1Sql(Tag, MysqlPid, ComF, T).

%%%===================================================================
%% @doc
%% 出错时写操作
%% @end
%%%===================================================================
writeError(MysqlPid, Tag, Key, Error) ->
    case writeSql:form_error_data({Tag, Key}, Error) of
        {ok, SqlBin, ValueL} ->
            Result = gen_server:call(MysqlPid, {'query_by_sql', {SqlBin, ValueL, 10000}}, infinity),
            [logger:error("mod:~p line:~p error:~p~n~p~n~p", [?MODULE, ?LINE, Result, SqlBin, ValueL]) || Result =/= ok];
        _ -> ok
    end.
