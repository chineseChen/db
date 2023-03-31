%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2022, <joinGames>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2022 17:17
%%%-------------------------------------------------------------------
-module(writeSql).
-author("ckc").
-include("db_channel.hrl").
-record(runArg, {
    key = none :: tuple(),%%当前key
    loopNum = 0 :: integer(), %%循环
    count = 0 :: integer(), %%内容条数
    mulKeyBool = false :: boolean(), %%是否为复合key
    cacheMgr = none :: tuple(), %%
    handle = none :: atom(), %%操类型
    cacheKeyL = [] :: list() %%所有key
}).

-define(DB_SQL_WRITE_STATE_DOING, '$db_sql_write_state_doing').
-define(DB_SQL_WRITE_CON_PID, '$db_sql_write_con_pid').
-define(DB_WRITE_SQL_KEY_SET, '$db_write_sql_key_set').
-define(BATCH_MYSQL_QUERY_TIME_OUT, 30000).
-define(BATCH_MYSQL_QUERY_LOCK_KEY, '$batch_mysql_query_lock_key').

%% API
-export([init/1]).
-export([tryStart/0, start/0]).
-export([formDeleteSql/2, formReplace/2]).
-export([formDeleteSql/3, formReplace/4]).
-export([form_error_data/2]).
-export([queryBySql/1, ping/0]).
-export([cleanEnv/0, cleanProcState/0]).
-export([deleteProcState/0]).
-export([getHandle/4]).

%%%===================================================================
%% @doc
%% 初始化
%% @end
%%%===================================================================
init(ConCfg) ->
    EtsRef = ets:new(?MODULE, [set, protected, {keypos, 1}]),
    {ok, MySqlCon} = mysql:start_link(ConCfg),
    put(?DB_SQL_WRITE_CON_PID, MySqlCon),
    put(?DB_WRITE_SQL_KEY_SET, EtsRef),
    [start() || whereis('db_cache_mgr') =/= undefined],
    ok.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
ping() ->
    get(?DB_SQL_WRITE_CON_PID) ! ping,
    ok.

%%%%===================================================================
%% @doc
%% 试着开始
%% @end
%%%===================================================================
tryStart() ->
    [start() || not ets:member(?DB_WRITE_PROC_STATE_ETS, self())].

%%%===================================================================
%% @doc
%% 清除ets进程状
%% @end
%%%===================================================================
cleanProcState() ->
    [ets:delete(?DB_WRITE_PROC_STATE_ETS, self()) || erlang:process_info(self(), message_queue_len) =:= {message_queue_len, 0}],
    ok.

%%%===================================================================
%% @doc
%% 删除proc
%% @end
%%%===================================================================
deleteProcState() ->
    ets:delete(?DB_WRITE_PROC_STATE_ETS, self()).

%%%===================================================================
%% @doc
%% 执行查询
%% @end
%%%===================================================================
queryBySql(Arg) ->
    try
        ConPid = get(?DB_SQL_WRITE_CON_PID),
        case Arg of
            {Sql, TimeOut} ->
                mysql:query(ConPid, Sql, TimeOut);
            {Sql, ValueL, TimeOut} ->
                mysql:query(ConPid, Sql, ValueL, TimeOut)
        end
    catch
        EType:EReason:Strace ->
            logger:error("mod:~p line:~p error:~p ~p~n~p", [?MODULE, ?LINE, EType, EReason, Strace]),
            error
    end.

%%%===================================================================
%% @doc
%% 开始
%% @end
%%%===================================================================
start() ->
    {Count, LoopNum} = application:get_env(c_db, mysqlBatchNum, {100, 10000}),
    F = fun
        (_, '$end_of_table') -> {break, '$end_of_table'};
        (_, ChangeId) ->
            case gen_server:call(writeSqlMgr, {'take_1', ChangeId, self()}) of
                '$continue' -> ets:next(?ETS_DB_CACHE_CHANGE, ChangeId);
                {ok, {Tag, MulKey}} ->
                    #cacheMgr{mod = Mod} = CacheMgr = db_cache_m:tab_get(Tag),
                    Key = ?BOOL_VALUE(db_data:isMulKey(Mod), element(1, MulKey), MulKey),
                    case ets:lookup(CacheMgr#cacheMgr.etsCacheInfo, Key) of
                        [#cacheInfo{running = #running{}}] ->
                            takeSql:delLock([{Tag, MulKey}]),
                            {ok, ets:next(?ETS_DB_CACHE_CHANGE, ChangeId)};
                        _ ->
                            case ets:lookup(CacheMgr#cacheMgr.etsChangeInfo, MulKey) of
                                [#changeInfo{change_ref = ChangeRef}] ->
                                    Handle = getHandle(CacheMgr, MulKey),
                                    ets:insert(get(?DB_WRITE_SQL_KEY_SET), {{Tag, MulKey}}),
                                    NChangeId = ets:next(CacheMgr#cacheMgr.etsChange, ChangeId),
                                    RunArg0 = #runArg{key = NChangeId, cacheMgr = CacheMgr,
                                        mulKeyBool = db_data:isMulKey(Mod), handle = Handle,
                                        cacheKeyL = [{MulKey, ChangeRef}]},
                                    {break, {ok, RunArg0}};
                                [] ->
                                    takeSql:delLock([{Tag, MulKey}]),
                                    {ok, ets:next(?ETS_DB_CACHE_CHANGE, ChangeId)}
                            end
                    end
            end
    end,
    case c_lib:for(F, ets:first(?ETS_DB_CACHE_CHANGE), 0, LoopNum) of
        {ok, RunArg} ->
            ets:insert(?DB_WRITE_PROC_STATE_ETS, {self()}),
            NRunArg = RunArg#runArg{count = Count - 1, loopNum = LoopNum},
            doLoop(NRunArg);
        '$end_of_table' ->
            ets:delete(?DB_WRITE_PROC_STATE_ETS, self());
        _ ->
            gen_server:cast(self(), 'try_write_start')
    end.

%%%===================================================================
%% @doc
%% 获取操作类型
%% @end
%%%===================================================================
getHandle(CacheMgr, MulKey) ->
    getHandle(CacheMgr#cacheMgr.mod, CacheMgr#cacheMgr.etsKey, CacheMgr#cacheMgr.etsCache, MulKey).
getHandle(Mod, EtsKey, EtsCache, MulKey) ->
    case db_data:isMulKey(Mod) of
        true ->
            Key = element(1, MulKey),
            case ets:lookup(EtsKey, Key) of
                [] -> 'delete';
                _ ->
                    case ets:lookup(EtsCache, MulKey) of
                        [] -> 'delete';
                        [_ | _] -> 'replace'
                    end
            end;
        false ->
            case ets:lookup(EtsKey, MulKey) of
                [] -> 'delete';
                _ -> 'replace'
            end
    end.

%%%===================================================================
%% @doc
%% 循环获取多个数据
%% @end
%%%===================================================================
doLoop(#runArg{loopNum = LoopNum} = RunArg) when LoopNum =< 0 ->
    batch(RunArg);
doLoop(#runArg{count = Count} = RunArg) when Count =< 0 ->
    batch(RunArg);
doLoop(#runArg{key = '$end_of_table'} = RunArg) ->
    batch(RunArg);
doLoop(RunArg) ->
    #runArg{cacheMgr = CacheMgr, key = ChangeId} = RunArg,
    ChangeTab = CacheMgr#cacheMgr.etsChange,
    case ets:lookup(ChangeTab, ChangeId) of
        [] ->
            doLoop(RunArg#runArg{key = ets:next(ChangeTab, ChangeId), loopNum = RunArg#runArg.loopNum - 1});
        [#cacheChange{key = MulKey}] ->
            CacheKey = ?BOOL_VALUE(RunArg#runArg.mulKeyBool, element(1, MulKey), MulKey),
            case ets:lookup(CacheMgr#cacheMgr.etsCacheInfo, CacheKey) of
                [#cacheInfo{running = #running{}}] ->
                    doLoop(RunArg#runArg{key = ets:next(ChangeTab, ChangeId), loopNum = RunArg#runArg.loopNum - 1});
                _ ->
                    case ets:lookup(CacheMgr#cacheMgr.etsChangeInfo, MulKey) of
                        [] ->
                            ets:delete(ChangeTab, ChangeId),
                            doLoop(RunArg#runArg{key = ets:next(ChangeTab, ChangeId), loopNum = RunArg#runArg.loopNum - 1});
                        [#changeInfo{change_ref = ChangeRef}] ->
                            Handle = RunArg#runArg.handle,
                            case getHandle(CacheMgr, MulKey) of
                                Handle ->
                                    Tag = CacheMgr#cacheMgr.tag,
                                    Msg = ?BOOL_VALUE(RunArg#runArg.mulKeyBool,
                                        {'takeMulKey_2', {ChangeTab, Tag, ChangeId, self()}},
                                        {'take_2', {ChangeTab, Tag, ChangeId}}),
                                    case gen_server:call(writeSqlMgr, Msg) of
                                        ok ->
                                            TabKey = {Tag, MulKey},
                                            ets:insert(get(?DB_WRITE_SQL_KEY_SET), {TabKey}),
                                            NRunArg1 = RunArg#runArg{
                                                loopNum = RunArg#runArg.loopNum - 1,
                                                count = RunArg#runArg.count - 1,
                                                cacheKeyL = [{MulKey, ChangeRef} | RunArg#runArg.cacheKeyL]
                                            },
                                            doLoop(NRunArg1);
                                        '$continue' ->
                                            doLoop(RunArg#runArg{key = ets:next(ChangeTab, ChangeId), loopNum = RunArg#runArg.loopNum - 1})
                                    end;
                                _ ->
                                    doLoop(RunArg#runArg{key = ets:next(ChangeTab, ChangeId), loopNum = RunArg#runArg.loopNum - 1})
                            end
                    end
            end
    end.

%%%===================================================================
%% @doc
%% 批量安全执行
%% @end
%%%===================================================================
batch(RunArg) ->
    try
        doBatch(RunArg)
    catch
        EType:EReason:Strace ->
            logger:error("mod:~p line:~p error:~p reason:~p~n~p", [?MODULE, ?LINE, EType, EReason, Strace]),
            CacheMgr = RunArg#runArg.cacheMgr,
            over(CacheMgr#cacheMgr.tag, RunArg#runArg.cacheKeyL, false)
    end.

%%%===================================================================
%% @doc
%% 批量执行
%% @end
%%%===================================================================
doBatch(#runArg{cacheKeyL = []}) -> ok;
doBatch(#runArg{cacheMgr = #cacheMgr{tag = SupTag}, handle = 'delete', cacheKeyL = L}) ->
    ConPid = get(?DB_SQL_WRITE_CON_PID),
    Sql = formDeleteSql(SupTag, L),
    Result = mysql:query(ConPid, Sql, ?BATCH_MYSQL_QUERY_TIME_OUT),
    [begin
        {DBSrc, _} = SupTag,
        DBName = atom_to_binary(gen_server:call('db_sql_mgr', {'get_db_name', DBSrc})),
        Mod = db_cache_m:tab_get(SupTag, #cacheMgr.mod),
        F = fun(Value) -> formDeleteSql(Mod, DBName, [Value]) end,
        write1Sql(SupTag, ConPid, F, L)
    end || Result =/= ok],
    over(SupTag, L),
    ok;
doBatch(#runArg{cacheMgr = #cacheMgr{tag = SupTag}, handle = 'replace', cacheKeyL = L}) ->
    Sql = formReplace(SupTag, L),
    [begin
        ConPid = get(?DB_SQL_WRITE_CON_PID),
%%        T1 = erlang:monotonic_time(),
        Result = mysql:query(ConPid, Sql, ?BATCH_MYSQL_QUERY_TIME_OUT),
%%        T2 = erlang:monotonic_time(),
%%        Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
%%        logger:debug("mod:~p time:~p", [SupTag, Time]),
        [begin
            CacheTab = db_cache_m:tab_get(SupTag, #cacheMgr.etsCache),
            {DBSrc, _} = SupTag,
            DBName = atom_to_binary(gen_server:call('db_sql_mgr', {'get_db_name', DBSrc})),
            Mod = db_cache_m:tab_get(SupTag, #cacheMgr.mod),
            F = fun(Value) -> formReplace(Mod, DBName, CacheTab, [Value]) end,
            write1Sql(SupTag, ConPid, F, L)
        end || Result =/= ok]
    end || is_binary(Sql)],
    over(SupTag, L),
    ok.

%%%===================================================================
%% @doc
%% 出错后复写
%% @end
%%%===================================================================
write1Sql(_Tag, _ConPid, _F, []) -> ok;
write1Sql(Tag, ConPid, F, [H | T]) ->
    Sql = F(H),
    [begin
        Result = mysql:query(ConPid, Sql, 10000),
        [begin
            writeError(ConPid, Tag, element(1, H), Result),
            logger:error("mod:~p line:~p error:~p~n~p", [?MODULE, ?LINE, Result, Sql])
        end || Result =/= ok]
    end || is_binary(Sql)],
    write1Sql(Tag, ConPid, F, T).

%%%===================================================================
%% @doc
%% 出错时写操作
%% @end
%%%===================================================================
writeError(ConPid, Tag, Key, Error) ->
    case form_error_data({Tag, Key}, Error) of
        {ok, SqlBin, ValueL} ->
            Result = mysql:query(ConPid, SqlBin, ValueL, 10000),
            [logger:error("mod:~p line:~p error:~p~n~p~n~p", [?MODULE, ?LINE, Result, SqlBin, ValueL]) || Result =/= ok];
        _ -> ok
    end.

%%%===================================================================
%% @doc
%% 最后的清理
%% @end
%%%===================================================================
over(Tag, L) ->
    over(Tag, L, true).
over(Tag, L, Bool) ->
    ets:delete_all_objects(get(?DB_WRITE_SQL_KEY_SET)),
    TabKeyL = [{Tag, Key} || {Key, _} <- L],
    takeSql:delLock(TabKeyL),
    [begin
        Pid = db_cache_m:tab_get(Tag, #cacheMgr.pid),
        gen_server:cast(Pid, {'update_cache_info', L})
    end || Bool],
    ets:delete(?DB_WRITE_PROC_STATE_ETS, self()),
    gen_server:cast(self(), 'try_write_start'),
    ok.

%%%===================================================================
%% @doc
%% 退出时执行
%% @end
%%%===================================================================
cleanEnv() ->
    EtsName = get(?DB_WRITE_SQL_KEY_SET),
    F = fun({{Tag, Key} = TabKey}, {_, R}) ->
        ets:delete(EtsName, TabKey),
        {Tag, [{Key, none} | R]}
    end,
    {RTag, L} = ets:foldl(F, {none, []}, EtsName),
    [begin
        Pid = db_cache_m:tab_get(RTag, #cacheMgr.pid),
        gen_server:cast(Pid, {'update_cache_info', L})
    end || RTag =/= none],
    ok.

%%%===================================================================
%% @doc
%% 构造删除数据
%% @end
%%%===================================================================
formDeleteSql(SupTag, L) ->
    {DBSrc, _} = SupTag,
    DBName = atom_to_binary(gen_server:call('db_sql_mgr', {'get_db_name', DBSrc})),
    Mod = db_cache_m:tab_get(SupTag, #cacheMgr.mod),
    formDeleteSql(Mod, DBName, L).
formDeleteSql(Mod, DBName, L) ->
    PriKey = Mod:primary_key(),
    case is_tuple(PriKey) of
        true -> formDeleteSql_l(Mod, DBName, L);
        false -> formDeleteSql_1(Mod, DBName, PriKey, L)
    end.

%%%===================================================================
%% @doc
%% 删除多键表
%% @end
%%%===================================================================
formDeleteSql_l(Mod, DBName, L) ->
    IndexT = Mod:key_index(),
    SqlTab = Mod:table_name(),
    Sql0 = <<"DELETE FROM ", DBName/binary, $., SqlTab/binary, " WHERE ">>,
    Size = tuple_size(IndexT),
    F = fun({MulKey, _}, Acc) ->
        FF = fun(Con, I, Index) ->
            {Field, ErlType, _, _} = Mod:info_by_index(Index),
            Value = db_sql_data:value2SqlVarByType(element(I, MulKey), ErlType),
            Bin0 = <<Field/binary, $=, Value/binary>>,
            Bin = ?BOOL_VALUE(I =/= Size, <<Bin0/binary, $&, $&>>, Bin0),
            <<Bin/binary, Con/binary>>
        end,
        ConBin = c_lib:tuple_foreach(FF, <<>>, IndexT),
        [ConBin | Acc]
    end,
    case lists:foldl(F, [], L) of
        [Con1Bin] -> <<Sql0/binary, Con1Bin/binary, $;>>;
        ConL ->
            F1 = fun(I, {[H | T], Sql1}) ->
                ConBin0 = <<$(, H/binary, $)>>,
                ConBin = case I =:= 1 of
                    true -> ConBin0;
                    false -> <<ConBin0/binary, $|, $|>>
                end,
                {T, <<ConBin/binary, Sql1/binary>>}
            end,
            {_, Sql3} = c_lib:for(F1, {ConL, <<>>}, 1, length(ConL) + 1),
            <<Sql0/binary, Sql3/binary, $;>>
    end.

%%%===================================================================
%% @doc
%% 删除单键表
%% @end
%%%===================================================================
formDeleteSql_1(Mod, DBName, PriKey, L) ->
    SqlTab = Mod:table_name(),
    Sql0 = <<"DELETE FROM ", DBName/binary, $., SqlTab/binary, " WHERE ", PriKey/binary, " IN (">>,
    {_, ErlType, _, _} = Mod:info_by_index(Mod:key_index()),
    Max = length(L),
    F = fun({Key, _}, {I, Bin}) ->
        Value = db_sql_data:value2SqlVarByType(Key, ErlType),
        Bin1 = <<Bin/binary, Value/binary>>,
        NBin = ?BOOL_VALUE(Max =/= I, <<Bin1/binary, $,>>, Bin1),
        {I + 1, NBin}
    end,
    {_, Sql1} = lists:foldl(F, {1, Sql0}, L),
    <<Sql1/binary, $), $;>>.

%%%===================================================================
%% @doc
%% 批量替换
%% @end
%%%===================================================================
formReplace(SupTag, L) ->
    CacheTab = db_cache_m:tab_get(SupTag, #cacheMgr.etsCache),
    {DBSrc, _} = SupTag,
    DBName = atom_to_binary(gen_server:call('db_sql_mgr', {'get_db_name', DBSrc})),
    Mod = db_cache_m:tab_get(SupTag, #cacheMgr.mod),
    formReplace(Mod, DBName, CacheTab, L).
formReplace(Mod, DBName, CacheTab, L) ->
    ValueL = lists:foldl(fun({Key, _}, Acc) ->
        case db_data:isMulKey(Mod) of
            true -> [Var || {_, Var} <- ets:lookup(CacheTab, Key)] ++ Acc;
            false -> ets:lookup(CacheTab, Key) ++ Acc
        end
    end, [], L),
    case ValueL =/= [] of
        true ->
            SqlTab = Mod:table_name(),
            Sql0 = <<"REPLACE INTO ", DBName/binary, $., SqlTab/binary, $(>>,
            RecSize = size(Mod:init()),
            Sql1 = formField(Mod, Sql0, RecSize),
            Sql2 = <<Sql1/binary, ") VALUES ">>,
            Sql3 = formValues(Mod, Sql2, RecSize, ValueL),
            <<Sql3/binary, $;>>;
        false -> none
    end.

%%%===================================================================
%% @doc
%% 构造field
%% @end
%%%===================================================================
formField(Mod, Sql, RecSize) ->
    FieldF = fun(I, Bin) ->
        case Mod:info_by_index(I) of
            none -> Bin;
            Tuple ->
                FieldStr = element(1, Tuple),
                Bin1 = <<Bin/binary, FieldStr/binary>>,
                ?BOOL_VALUE(RecSize =/= I, <<Bin1/binary, $,>>, Bin1)
        end
    end,
    Sql1 = c_lib:for(FieldF, Sql, 1, RecSize + 1),
    Sql1.

%%%===================================================================
%% @doc
%%  构造values
%% @end
%%%===================================================================
formValues(Mod, Sql, RecSize, ValueL) ->
    Length = length(ValueL),
    F = fun(Value, {Index, Bin}) ->
        Bin1 = <<Bin/binary, $(>>,
        FF = fun(I, Bin2) ->
            case Mod:info_by_index(I) of
                none -> Bin2;
                {_, ErlType, _, _} ->
                    Value1 = db_sql_data:value2SqlVarByType(element(I, Value), ErlType),
                    Bin3 = <<Bin2/binary, Value1/binary>>,
                    ?BOOL_VALUE(I =/= RecSize, <<Bin3/binary, $,>>, Bin3)
            end
        end,
        NBin0 = c_lib:for(FF, Bin1, 1, RecSize + 1),
        NBin1 = <<NBin0/binary, $)>>,
        NBin = ?BOOL_VALUE(Index =/= Length, <<NBin1/binary, $,>>, NBin1),
        {Index + 1, NBin}
    end,
    {_, NSql} = lists:foldl(F, {1, Sql}, ValueL),
    NSql.

%%%===================================================================
%% @doc
%%  构造错误数据
%% @end
%%%===================================================================
form_error_data(DbKey, Res) ->
    case o_tuple:get(?DB_WRITE_ERROR_TAB_TAG, dbT:get_mod()) of
        {_, Mod} when is_atom(Mod) ->
            {{DBSrc, _} = STag, Key} = DbKey,
            CacheTab = db_cache_m:tab_get(STag, #cacheMgr.etsCache),
            Value = case ets:lookup(CacheTab, Key) of
                [Value0] -> Value0;
                [] -> none
            end,
            DBName = gen_server:call('db_sql_mgr', {'get_db_name', DBSrc}),
            ErrorValue0 = Mod:init(unicode:characters_to_binary(str_lib:term_2_str(DbKey))),
            ErrorValue1 = Mod:set_error(ErrorValue0, unicode:characters_to_binary(str_lib:term_2_str(Res))),
            ErrorValue2 = Mod:set_value(ErrorValue1, unicode:characters_to_binary(str_lib:term_2_str(Value))),
            ErrorValue = Mod:set_time(ErrorValue2, c_time:now_second()),
            {Str0, ValueL, _} = db_sql_data:form(Mod, ErrorValue, all),
            Str = io_lib:format("REPLACE INTO ~s.`~s` ~s;", [DBName, ?DB_WRITE_ERROR_TAB_TAG, Str0]),
            SqlBin = unicode:characters_to_binary(Str),
            {ok, SqlBin, ValueL};
        _ -> none
    end.
