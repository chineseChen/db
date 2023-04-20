%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 14. 9月 2021 15:19
%%%-------------------------------------------------------------------
-module(db_cache).
-author("chenkecai").
-include("db_channel.hrl").
-define(TIME_LOOP_SAVE_INTERVAL, 200).
-define(TIME_LOOP_SAVE_ONCE_COUNT, 1000).
-define(ETS_DB_CACHE_CHANGE_MAX_SIZE, 16#3E80000). %%500MB

-define(DB_ETS_KEY_TAB, '$key'). %%所有key值表
-define(DB_ETS_INDEX_TAB, '$index'). %%所有key值表
-define(DB_ETS_CACHE_TAB, '$cache'). %%缓存表
-define(DB_ETS_CACHE_INFO_TAB, '$cache_info'). %%缓存变化信息
-define(DB_ETS_CHANGE_TAB, '$change'). %%时间变化排序系列,用于持久化
-define(DB_ETS_CHANGE_INFO_TAB, '$change_info'). %%时间变化排序系列,用于持久化
-define(DB_ETS_TAB_LOCK_QUEUE, '$tab_lock_queue'). %%表锁信息
-define(DB_ETS_TAB_LOCK_RUN, '$tab_lock_run'). %%表中正执行的数据

-define(DB_CACHE_TAG, '$cache_tag'). %%cache对应tag
-define(DB_CACHE_MOD, '$cache_mod'). %%cache对应mod
-define(DB_CACHE_DISK_BOOL, '$diskBool'). %%cache对应mod
-define(DB_CACHE_MUL_PRI_KEY, '$mul_pri_key'). %%cache对应mod

-define(CACHE_LOOP_CLEAR_ETS_KEY, '$loop_clear_ets_key'). %%定期清理缓存,循环时中间值
-define(CACHE_LOOP_SAVE_ETS_KEY, '$save_loop_key'). %%定期存储缓存

%% API
-export([getIndexData/1]).
-export([setSupTagMod/3, getCacheTag/0, formatArg/0, maxChangeIndex/0]).
-export([new_tab/2, init_key/0, insert/1, writeStart/1, insertData/1, writeEnd/1, rollBack/1,
    reCalcCacheChange/0, terminate/0, update_cache_info/1]).
-export([timerLoopSave/0, timerLoopClear/1]).
-export([modifyChangeTime/4]).
-export([clearData/1]).

%%%===================================================================
%% @doc
%%  创建表
%% @end
%%%===================================================================
new_tab(none, Arg) ->
    CatchInfoRef = ets:new(?DB_ETS_CACHE_INFO_TAB, [ordered_set, protected, {keypos, #cacheInfo.key}]),
    CatchTRef = ets:new(?DB_ETS_CACHE_TAB, [ordered_set, protected, {keypos, 1}]),
    LockTRef = ets:new(?DB_ETS_TAB_LOCK_QUEUE, [ordered_set, public, {keypos, #lock_queue.uid}]),
    RunTRef = ets:new(?DB_ETS_TAB_LOCK_RUN, [set, public, {keypos, #lock_run.pid}]),
    IndexRefL = dbIndex:createTab(element(2, Arg#cacheMgr.tag)),
    put(?DB_ETS_CACHE_INFO_TAB, CatchInfoRef),
    put(?DB_ETS_CACHE_TAB, CatchTRef),
    put(?DB_ETS_TAB_LOCK_QUEUE, LockTRef),
    put(?DB_ETS_TAB_LOCK_RUN, RunTRef),
    put(?DB_CACHE_DISK_BOOL, false),
    put(?DB_ETS_INDEX_TAB, IndexRefL),
    Arg#cacheMgr{diskBool = false, etsIndex = IndexRefL, etsCache = CatchTRef, etsCacheInfo = CatchInfoRef,
        etsLockQueue = LockTRef, etsLock = RunTRef};
new_tab(Mod, Arg) when is_atom(Mod) ->
    KeyIndex = Mod:key_index(),
    CatchInfoRef = ets:new(?DB_ETS_CACHE_INFO_TAB, [ordered_set, protected, {keypos, #cacheInfo.key}]),
    LockTRef = ets:new(?DB_ETS_TAB_LOCK_QUEUE, [ordered_set, public, {keypos, #lock_queue.uid}]),
    RunTRef = ets:new(?DB_ETS_TAB_LOCK_RUN, [set, public, {keypos, #lock_run.pid}]),
    IndexRefL = dbIndex:createTab(element(2, Arg#cacheMgr.tag)),
    put(?DB_ETS_CACHE_INFO_TAB, CatchInfoRef),
    put(?DB_ETS_TAB_LOCK_QUEUE, LockTRef),
    put(?DB_ETS_TAB_LOCK_RUN, RunTRef),
    put(?DB_ETS_INDEX_TAB, IndexRefL),
    Arg0 = Arg#cacheMgr{etsIndex = IndexRefL, etsCacheInfo = CatchInfoRef, etsLockQueue = LockTRef, etsLock = RunTRef},
    case Mod:save_type() of
        'disk' ->
            CatchTRef = case is_tuple(KeyIndex) of
                true ->
                    put(?DB_CACHE_MUL_PRI_KEY, true),
                    ets:new(?DB_ETS_CACHE_TAB, [ordered_set, protected, {keypos, 1}]);
                false ->
                    ets:new(?DB_ETS_CACHE_TAB, [set, protected, {keypos, KeyIndex}])
            end,
            KeyTRef = ets:new(?DB_ETS_KEY_TAB, [ordered_set, protected, {keypos, 1}]),
            ChangeRef = ets:new(?DB_ETS_CHANGE_TAB, [ordered_set, public, protected, {keypos, #cacheChange.id}]),
            ChangeInfoTRef = ets:new(?DB_ETS_CHANGE_INFO_TAB, [ordered_set, protected, {keypos, #changeInfo.key}]),
            put(?DB_CACHE_DISK_BOOL, true),
            put(?DB_ETS_KEY_TAB, KeyTRef), put(?DB_ETS_CACHE_TAB, CatchTRef), put(?DB_ETS_CHANGE_TAB, ChangeRef),
            put(?DB_ETS_CHANGE_INFO_TAB, ChangeInfoTRef),
            Arg0#cacheMgr{
                diskBool = true, etsCache = CatchTRef, etsKey = KeyTRef, etsChange = ChangeRef,
                etsChangeInfo = ChangeInfoTRef
            };
        _ ->
            CatchTRef = ets:new(?DB_ETS_CACHE_TAB, [ordered_set, protected, {keypos, 1}]),
            put(?DB_ETS_CACHE_TAB, CatchTRef),
            Arg1 = Arg0#cacheMgr{etsCache = CatchTRef},
            Arg2 = case is_tuple(KeyIndex) of
                true ->
                    KeyTRef = ets:new(?DB_ETS_KEY_TAB, [ordered_set, protected, {keypos, 1}]),
                    put(?DB_ETS_KEY_TAB, KeyTRef),
                    put(?DB_CACHE_MUL_PRI_KEY, true),
                    Arg1#cacheMgr{etsKey = KeyTRef};
                false -> Arg1
            end,
            put(?DB_CACHE_DISK_BOOL, false),
            Arg2#cacheMgr{diskBool = false}
    end.

%%%===================================================================
%% @doc
%% 设进程基础信息
%% @end
%%%===================================================================
setSupTagMod(STag, Mod, Arg) ->
    put(?DB_CACHE_TAG, STag),
    put(?DB_CACHE_MOD, Mod),
    Arg#cacheMgr{tag = STag, mod = Mod}.

%%%===================================================================
%% @doc
%% 获取cache_tag
%% @end
%%%===================================================================
getCacheTag() ->
    get(?DB_CACHE_TAG).

%%%===================================================================
%% @doc
%% 构造arg
%% @end
%%%===================================================================
formatArg() ->
    Bool = get(?DB_CACHE_DISK_BOOL),
    #cacheMgr{
        tag = get(?DB_CACHE_TAG),
        mod = get(?DB_CACHE_MOD),
        pid = self(), diskBool = Bool,
        etsKey = ?BOOL_VALUE(Bool, get(?DB_ETS_KEY_TAB), none),
        etsCache = get(?DB_ETS_KEY_TAB),
        etsCacheInfo = get(?DB_ETS_CACHE_TAB),
        etsChange = ?BOOL_VALUE(Bool, get(?DB_ETS_CHANGE_TAB), none)
    }.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
maxChangeIndex() ->
    Bool = get(?DB_CACHE_DISK_BOOL),
    case Bool of
        true ->
            Ets = get(?DB_ETS_CHANGE_TAB),
            case ets:last(Ets) of
                '$end_of_table' -> 0;
                Uid -> Uid
            end;
        false -> 0
    end.

%%%===================================================================
%% @doc
%%  初始化key
%% @end
%%%===================================================================
init_key() ->
    [begin
        Mod = get(?DB_CACHE_MOD),
        {PriKey, KeyIndex} = case Mod:primary_key() of
            PriKeyT when is_tuple(PriKeyT) ->
                {element(1, PriKeyT), element(1, Mod:key_index())};
            PriKey0 ->
                {PriKey0, Mod:key_index()}
        end,
        {_, ErlType, _, _} = Mod:info_by_index(KeyIndex),
        {HStr, IErlTypeL} = formatHeadSql(Mod, PriKey),
        QueryStr1 = unicode:characters_to_binary(io_lib:format("~s order by ~s ASC limit ~w;", [HStr, PriKey, ?CACHE_INIT_FROM_SQL_PAGE_NUM])),
        IndexRefL = get(?DB_ETS_INDEX_TAB),
        IndexInfo = lists:zip(IndexRefL, IErlTypeL),
        MaxVar0 = initKeyByQuery(ErlType, IndexInfo, QueryStr1),
        F = fun
            (none) -> {break, ok};
            (MaxVar) ->
                QueryStr2 = unicode:characters_to_binary(io_lib:format("~s WHERE ~s > ~s order by ~s ASC limit ~w;",
                    [HStr, PriKey, formatWhereConVar(ErlType, MaxVar), PriKey, ?CACHE_INIT_FROM_SQL_PAGE_NUM])),
                initKeyByQuery(ErlType, IndexInfo, QueryStr2)
        end,
        c_lib:while(F, MaxVar0)
    end || get(?DB_CACHE_DISK_BOOL)].

%%%===================================================================
%% @doc
%% 构造头部sql
%% @end
%%%===================================================================
formatHeadSql(Mod, PriKey) ->
    {DbSrc, _} = get(?DB_CACHE_TAG),
    {ok, DBName0} = gen_server:call('db_sql_mgr', {'get_db_name', DbSrc}),
    DBName = atom_to_binary(DBName0),
    TableName = Mod:table_name(),
    case dbIndex:getPos2(Mod) of
        [] -> {<<"select ", PriKey/binary, " from ", DBName/binary, $., TableName/binary>>, []};
        PosL ->
            F = fun(Index, {AAcc, BAcc}) ->
                {Field, IErlType, _, _} = Mod:info_by_index(Index),
                {[Field, <<$,>> | AAcc], [IErlType | BAcc]}
            end,
            {BinL0, IErlTypeL} = lists:foldl(F, {[], []}, PosL),
            [_ | BinL] = lists:reverse(BinL0),
            IndexBin = list_to_binary(BinL),
            Sql = <<"select ", PriKey/binary, $,, IndexBin/binary, " from ", DBName/binary, $., TableName/binary>>,
            {Sql, lists:reverse(IErlTypeL)}
    end.

%%%===================================================================
%% @doc
%% 构造where条个
%% @end
%%%===================================================================
formatWhereConVar(ErlType, Var) ->
    case db_sql_data:isNumberByType(ErlType) of
        true -> lists:flatten(io_lib:format("~p", [Var]));
        false -> lists:flatten(io_lib:format("'~s'", [Var]))
    end.

%%%===================================================================
%% @doc
%% 初始化key表通过查询
%% @end
%%%===================================================================
initKeyByQuery(KErlType, IndexInfo, QueryStr) ->
    SqlPid = db_sql:getReadPid(),
    ResL = case catch mysql:query(SqlPid, QueryStr, 60000) of
        {ok, _, PriKeyL0} ->
            PriKeyL0;
        MysqlError ->
            error({MysqlError, QueryStr})
    end,
    F = fun([PriKeyVar | SubL], _) ->
        Key = db_sql_data:erl_value_by_type(PriKeyVar, KErlType),
        ets:insert(get(?DB_ETS_KEY_TAB), {Key}),
        FF = fun(_, {[{IndexRef, ErlType} | T1], [Var | T2]}) ->
            IndexVar = db_sql_data:erl_value_by_type(Var, ErlType),
            ets:insert(IndexRef, {IndexVar, Key}),
            {T1, T2}
        end,
        lists:foldl(FF, {IndexInfo, SubL}, SubL),
        PriKeyVar
    end,
    lists:foldl(F, none, ResL).

%%%===================================================================
%% @doc
%% 插入缓存数据
%% @end
%%%===================================================================
insert({Key, Value, NowMSec}) ->
    CatchTInfoRef = get(?DB_ETS_CACHE_INFO_TAB),
    [case ets:lookup('db_lock', {get(?DB_CACHE_TAG), Key}) of
        %%键被上锁中,不用管
        [#lock{pid = Pid, e_time = ETime}] when Pid =/= none andalso NowMSec < ETime -> ok;
        _ -> ets:insert(get(?DB_ETS_CACHE_TAB), Value)
    end || not ets:member(CatchTInfoRef, Key)],
    MulCount = ?BOOL_VALUE(is_list(Value), length(Value), 0),
    insert({Key, NowMSec}, MulCount),
    ok;
insert({Key, NowMSec}) ->
    insert({Key, NowMSec}, 0).
insert({Key, NowMSec}, MulCount) ->
    CatchTInfoRef = get(?DB_ETS_CACHE_INFO_TAB),
    case ets:member(CatchTInfoRef, Key) of
        true -> ets:update_element(CatchTInfoRef, Key, {#cacheInfo.active_time, NowMSec});
        false -> ets:insert(CatchTInfoRef, #cacheInfo{key = Key, mulCount = MulCount, active_time = NowMSec})
    end.

%%%===================================================================
%% @doc
%%  写数据
%% @end
%%%===================================================================
writeStart({Pid, {Key, Value, OldValue, ChangeL}}) ->
    case get(?DB_CACHE_DISK_BOOL) of
        true -> writeStart_1(Pid, Key, OldValue, Value, ChangeL);
        false -> writeStart_2(Pid, Key, OldValue, Value)
    end, ok.

%%%===================================================================
%% @doc
%% 插入数据
%% @end
%%%===================================================================
insertData({Pid, {Key, Value, ChangeL}}) ->
    KeyTRef = get(?DB_ETS_KEY_TAB),
    case get(?DB_CACHE_DISK_BOOL) of
        true ->
            case ets:member(KeyTRef, Key) of
                true -> "only insert new data";
                false -> writeStart_1(Pid, Key, none, Value, ChangeL), ok
            end;
        false ->
            case KeyTRef =/= undefined of
                true ->
                    case ets:member(KeyTRef, Key) of
                        true -> "only insert new data";
                        false -> writeStart_2(Pid, Key, none, Value), ok
                    end;
                false ->
                    CatchTRef = get(?DB_ETS_CACHE_TAB),
                    case ets:member(CatchTRef, Key) of
                        true -> "only insert new data";
                        false -> writeStart_2(Pid, Key, none, Value), ok
                    end
            end
    end.

%%%===================================================================
%% @doc
%%  写持久数据表
%% @end
%%%===================================================================
writeStart_1(Pid, Key, OldValue, Value, L) ->
    NowMSec = c_time:now_millisecond(),
    KeyTRef = get(?DB_ETS_KEY_TAB),
    CatchTRef = get(?DB_ETS_CACHE_TAB),
    CacheInfoTRef = get(?DB_ETS_CACHE_INFO_TAB),
    ChangeInfoTRef = get(?DB_ETS_CHANGE_INFO_TAB),
    Running = #running{pid = Pid, data = OldValue},
    MulCount = ?BOOL_VALUE(is_list(Value), length(Value), 0),
    Mod = get(?DB_CACHE_MOD),
    ets:insert(CacheInfoTRef, #cacheInfo{key = Key, running = Running, mulCount = MulCount, active_time = NowMSec}),
    case Value of
        'delete' ->
            ets:delete(KeyTRef, Key),
            ?BOOL_VALUE(db_data:isMulKey(Mod), db_data:delMulData(Mod, CatchTRef, Key), ets:delete(CatchTRef, Key)),
            delIndexTab(OldValue, Mod);
        _ ->
            ets:insert(KeyTRef, {Key}),
            [db_data:delMulData(Mod, CatchTRef, Key) || db_data:isMulKey(Mod)],
            ets:insert(CatchTRef, Value),
            modifyIndexTab(Key, OldValue, Value, Mod)
    end,
    ChangeL = [#changeInfo{key = MulKey, change_ref = erlang:make_ref()} || MulKey <- L],
    ets:insert(ChangeInfoTRef, ChangeL),
    ok.

%%%===================================================================
%% @doc
%%  写内存表
%% @end
%%%===================================================================
writeStart_2(Pid, Key, OldValue, Value) ->
    CatchTRef = get(?DB_ETS_CACHE_TAB),
    CacheInfoTRef = get(?DB_ETS_CACHE_INFO_TAB),
    KeyTRef = get(?DB_ETS_KEY_TAB),
    Running = #running{pid = Pid, data = OldValue},
    ets:insert(CacheInfoTRef, #cacheInfo{key = Key, running = Running}),
    Mod = get(?DB_CACHE_MOD),
    case Value of
        'delete' ->
            [ets:delete(KeyTRef, Key) || KeyTRef =/= undefined],
            ?BOOL_VALUE(db_data:isMulKey(Mod), db_data:delMulData(Mod, CatchTRef, Key), ets:delete(CatchTRef, Key)),
            delIndexTab(OldValue, Mod);
        _ ->
            [ets:insert(KeyTRef, {Key}) || KeyTRef =/= undefined],
            [db_data:delMulData(Mod, CatchTRef, Key) || db_data:isMulKey(Mod)],
            ets:insert(CatchTRef, Value),
            modifyIndexTab(Key, OldValue, Value, Mod)
    end, ok.

%%%===================================================================
%% @doc
%% 删除索引
%% @end
%%%===================================================================
delIndexTab(none, _) -> ok;
delIndexTab(_, none) -> ok;
delIndexTab(Value, Mod) ->
    case dbIndex:getPos2(Mod) of
        [] -> ok;
        IndexL -> delIndexTab_(Mod, IndexL, Value)
    end.

%%%===================================================================
%% @doc
%% 执行删除索引
%% @end
%%%===================================================================
delIndexTab_(Mod, IndexL, Value) ->
    F = fun(Ref, [Index | T]) ->
        case db_data:isMulKey(Mod) of
            true -> [ets:delete(Ref, element(Index, Var)) || Var <- Value];
            false -> ets:delete(Ref, element(Index, Value))
        end, T
    end,
    lists:foldl(F, IndexL, get(?DB_ETS_INDEX_TAB)).

%%%===================================================================
%% @doc
%% 修改索引
%% @end
%%%===================================================================
modifyIndexTab(_, _, _, none) -> ok;
modifyIndexTab(Key, OValue, NValue, Mod) ->
    case dbIndex:getPos2(Mod) of
        [] -> ok;
        IndexL -> modifyIndexTab_(Key, IndexL, OValue, NValue, Mod)
    end.

%%%===================================================================
%% @doc
%% 执行修改索引
%% @end
%%%===================================================================
modifyIndexTab_(Key, IndexL, OValue, NValue, Mod) ->
    F = fun(Ref, [Index | T]) ->
        case db_data:isMulKey(Mod) of
            true ->
                [[ets:delete(Ref, element(Index, Var)) || Var <- OValue] || OValue =/= none],
                [[ets:insert(Ref, {element(Index, Var), Key}) || Var <- NValue] || NValue =/= 'delete'];
            false ->
                [ets:delete(Ref, element(Index, OValue)) || OValue =/= none],
                [ets:insert(Ref, {element(Index, NValue), Key}) || NValue =/= 'delete']
        end, T
    end,
    lists:foldl(F, IndexL, get(?DB_ETS_INDEX_TAB)).

%%%===================================================================
%% @doc
%% 获取key
%% @end
%%%===================================================================
getIndexData(F) -> F().

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
writeEnd({Pid, Key}) ->
    CacheInfoTRef = get(?DB_ETS_CACHE_INFO_TAB),
    DiskBool = get(?DB_CACHE_DISK_BOOL),
    case ets:lookup(CacheInfoTRef, Key) of
        [#cacheInfo{running = #running{pid = Pid}}] ->
            case DiskBool of
                true -> ets:update_element(CacheInfoTRef, Key, {#cacheInfo.running, none});
                false -> ets:delete(CacheInfoTRef, Key)
            end;
        _ -> ok
    end, ok.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
modifyChangeTime(NowMSec, KeyL) ->
    modifyChangeTime(NowMSec, KeyL, true).
modifyChangeTime(NowMSec, KeyL, Bool) ->
    CacheTag = get(?DB_CACHE_TAG),
    ChangeTab = get(?DB_ETS_CHANGE_TAB),
    modifyChangeTime(NowMSec, CacheTag, ChangeTab, KeyL, Bool).
modifyChangeTime(NowMSec, CacheTag, ChangeTab, KeyL) ->
    modifyChangeTime(NowMSec, CacheTag, ChangeTab, KeyL, true).
modifyChangeTime(NowMSec, CacheTag, ChangeTab, KeyL, WriteBool) ->
    case ets:info(?ETS_DB_CACHE_CHANGE, memory) >= ?ETS_DB_CACHE_CHANGE_MAX_SIZE of
        true ->
            gen_server:cast('db_cache_mgr', 're_calc_cache_change'), ok;
        false ->
            F = fun(Key) ->
                Id = db_cache_m:makeChangeTabId(),
                ets:insert(ChangeTab, #cacheChange{id = {NowMSec, Id}, key = Key}),
                ets:insert(?ETS_DB_CACHE_CHANGE, #cacheSupChange{id = {NowMSec, Id}, tag = CacheTag})
            end,
            lists:foreach(F, KeyL),
            [gen_server:cast('writeSqlMgr', 'writeBySql') || WriteBool],
            ok
    end.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
clearData(Src) ->
    case get(?DB_CACHE_DISK_BOOL) of
        true ->
            clearDiskData(Src);
        false ->
            clearMemData()
    end,
    ok.

%%%===================================================================
%% @doc
%% 清理disk数据
%% @end
%%%===================================================================
clearDiskData(Src) ->
    KeyTRef = get(?DB_ETS_KEY_TAB),
    [begin
        Mod = get(?DB_CACHE_MOD),
        TableName = Mod:table_name(),
        MysqlPid = db_sql:getReadPid(),
        {ok, DBName} = gen_server:call('db_sql_mgr', {'get_db_name', Src}),
        Sql = unicode:characters_to_binary(io_lib:format("truncate table ~s.~s;", [DBName, TableName])),
        ok = mysql:query(MysqlPid, Sql),
        CacheTRef = get(?DB_ETS_CACHE_TAB),
        CacheInfoTRef = get(?DB_ETS_CACHE_INFO_TAB),
        %% 先清数据库再清表,如果下面发生错误pid将重启也会清掉数据
        ets:delete_all_objects(KeyTRef),
        ets:delete_all_objects(CacheInfoTRef),
        ets:delete_all_objects(CacheTRef),
        cleanIndexTab(),
        delTabAllObj()
    end || ets:first(KeyTRef) =/= '$end_of_table'],
    ok.

%%%===================================================================
%% @doc
%% 清理内存数据
%% @end
%%%===================================================================
clearMemData() ->
    CacheTRef = get(?DB_ETS_CACHE_TAB),
    CacheInfoTRef = get(?DB_ETS_CACHE_INFO_TAB),
    KeyTRef = get(?DB_ETS_KEY_TAB),
    case KeyTRef =/= none of
        true ->
            [begin
                ets:delete_all_objects(KeyTRef),
                ets:delete_all_objects(CacheInfoTRef),
                ets:delete_all_objects(CacheTRef),
                cleanIndexTab()
            end || ets:first(KeyTRef) =/= '$end_of_table'];
        false ->
            [begin
                ets:delete_all_objects(CacheInfoTRef),
                ets:delete_all_objects(CacheTRef),
                cleanIndexTab()
            end || ets:first(CacheTRef) =/= '$end_of_table']
    end, ok.

%%%===================================================================
%% @doc
%% 清除索引表
%% @end
%%%===================================================================
cleanIndexTab() ->
    L = get(?DB_ETS_INDEX_TAB),
    [ets:delete_all_objects(Ref) || Ref <- L],
    ok.

%%%===================================================================
%% @doc
%%  删除所有数据打标记
%% @end
%%%===================================================================
delTabAllObj() ->
    ChangeRef = get(?DB_ETS_CHANGE_TAB),
    ets:delete_all_objects(ChangeRef),
    ChangeInfoRef = get(?DB_ETS_CHANGE_INFO_TAB),
    CacheTag = get(?DB_CACHE_TAG),
    NowMSec = time_lib:now_millisecond(),
    F = fun(Value, _) ->
        ets:insert(ChangeInfoRef, Value#changeInfo{change_ref = erlang:make_ref()}),
        Id = db_cache_m:makeChangeTabId(),
        ets:insert(ChangeRef, #cacheChange{id = {NowMSec, Id}, key = Value#changeInfo.key}),
        ets:insert(?ETS_DB_CACHE_CHANGE, #cacheSupChange{id = {NowMSec, Id}, tag = CacheTag}),
        true
    end,
    WriteBool = ets:foldl(F, false, ChangeInfoRef),
    [gen_server:cast('writeSqlMgr', 'writeBySql') || WriteBool],
    ok.


%%%===================================================================
%% @doc
%%  定期清于是过期缓存,临时数据,即回滚
%% @end
%%%===================================================================
timerLoopClear(Count) ->
    CatchTInfoRef = get(?DB_ETS_CACHE_INFO_TAB),
    CatchTRef = get(?DB_ETS_CACHE_TAB),
    NowMSec = c_time:now_millisecond(),
    CompareTime = get_clear_time(CatchTInfoRef, NowMSec),
    OldKey = get(?CACHE_LOOP_CLEAR_ETS_KEY),
    NowKey = ?BOOL_VALUE(OldKey =:= undefined, ets:first(CatchTInfoRef), OldKey),
    F = fun
        (_, '$end_of_table') -> {break, '$end_of_table'};
        (_, Key) ->
            case ets:lookup(CatchTInfoRef, Key) of
                [#cacheInfo{running = #running{} = Running}] ->
                    rollBack(Running, CatchTInfoRef, CatchTRef, Key);
                [#cacheInfo{running = none, active_time = ActiveTime}] when CompareTime > ActiveTime ->
                    [delUnActiveCache(Key, CatchTInfoRef, CatchTRef) || get(?DB_CACHE_DISK_BOOL)];
                _ -> ok
            end,
            ets:next(CatchTInfoRef, Key)
    end,
    case c_lib:for(F, NowKey, 0, Count) of
        '$end_of_table' -> erlang:erase(?CACHE_LOOP_CLEAR_ETS_KEY);
        NextKey -> erlang:put(?CACHE_LOOP_CLEAR_ETS_KEY, NextKey)
    end.

%%%===================================================================
%% @doc
%%  清除不活跃缓存
%% @end
%%%===================================================================
delUnActiveCache(Key, CatchTInfoRef, CatchTRef) ->
    ChangeInfo = get(?DB_ETS_CHANGE_INFO_TAB),
    case get(?DB_CACHE_MUL_PRI_KEY) of
        true ->
            Mod = get(?DB_CACHE_MOD),
            [begin
                db_data:delMulData(Mod, CatchTRef, Key),
                ets:delete(CatchTInfoRef, Key)
            end || db_data:isNoChangeData(Mod, ChangeInfo, Key)];
        _ ->
            [begin
                ets:delete(CatchTInfoRef, Key),
                ets:delete(CatchTRef, Key)
            end || ets:lookup(ChangeInfo, Key) =:= []]
    end.

%%%===================================================================
%% @doc
%% 回滚
%% @end
%%%===================================================================
rollBack({Pid, Key}) ->
    CacheInfoTRef = get(?DB_ETS_CACHE_INFO_TAB),
    case ets:lookup(CacheInfoTRef, Key) of
        [#cacheInfo{running = Running = #running{pid = Pid}}] ->
            CacheTRef = get(?DB_ETS_CACHE_TAB),
            doRollBack(Running#running.data, CacheInfoTRef, CacheTRef, Key);
        _ -> ok
    end.
rollBack(Running, CatchInfoRef, CatchTRef, Key) ->
    [doRollBack(Running#running.data, CatchInfoRef, CatchTRef, Key) || not is_process_alive(Running#running.pid)].
doRollBack(OldValue, CatchInfoRef, CatchTRef, Key) ->
    case get(?DB_CACHE_DISK_BOOL) of
        true -> rollBackDisk(CatchInfoRef, CatchTRef, Key, OldValue);
        false -> rollBackMem(CatchInfoRef, CatchTRef, Key, OldValue)
    end, ok.

%%%===================================================================
%% @doc
%% disk回滚
%% @end
%%%===================================================================
rollBackDisk(CatchInfoRef, CatchTRef, Key, Value) ->
    KeyTRef = get(?DB_ETS_KEY_TAB),
    Mod = get(?DB_CACHE_MOD),
    MulCount = case Value =:= none of
        true ->
            ets:delete(KeyTRef, Key),
            ?BOOL_VALUE(db_data:isMulKey(Mod), db_data:delMulData(Mod, CatchTRef, Key), ets:delete(CatchTRef, Key)),
            0;
        false ->
            ets:insert(KeyTRef, {Key}),
            [db_data:delMulData(Mod, CatchTRef, Key) || db_data:isMulKey(Mod)],
            ets:insert(CatchTRef, Value),
            ?BOOL_VALUE(is_list(Value), length(Value), 0)
    end,
    rollBackIndex(Mod, Key, Value, CatchTRef),
    ets:update_element(CatchInfoRef, Key, [{#cacheInfo.running, none}, {#cacheInfo.mulCount, MulCount}]),
    ok.

%%%===================================================================
%% @doc
%% 内存回滚
%% @end
%%%===================================================================
rollBackMem(CatchInfoRef, CatchTRef, Key, Value) ->
    KeyTRef = get(?DB_ETS_KEY_TAB),
    Mod = get(?DB_CACHE_MOD),
    case Value =:= none of
        true ->
            [ets:delete(KeyTRef, Key) || KeyTRef =/= undefined],
            ?BOOL_VALUE(db_data:isMulKey(Mod), db_data:delMulData(Mod, CatchTRef, Key), ets:delete(CatchTRef, Key));
        false ->
            [ets:insert(KeyTRef, {Key}) || KeyTRef =/= undefined],
            [db_data:delMulData(Mod, CatchTRef, Key) || db_data:isMulKey(Mod)],
            ets:insert(CatchTRef, Value)
    end,
    rollBackIndex(Mod, Key, Value, CatchTRef),
    ets:delete(CatchInfoRef, Key),
    ok.

%%%===================================================================
%% @doc
%% 回滚索引表
%% @end
%%%===================================================================
rollBackIndex(Mod, Key, RBValue0, CatchTRef) ->
    [case dbIndex:getPos2(Mod) of
        [] -> ok;
        IndexL ->
            Value = case db_data:getMulValue(Mod, CatchTRef, Key) of
                [] -> none;
                ValueL ->
                    case db_data:isMulKey(Mod) of
                        true -> ValueL;
                        false -> hd(ValueL)
                    end
            end,
            RBValue = ?BOOL_VALUE(RBValue0 =:= none, 'delete', RBValue0),
            modifyIndexTab_(Key, IndexL, Value, RBValue, Mod)
    end || Mod =/= none].

%%%===================================================================
%% @doc
%%  设置缓存变化信息
%% @end
%%%===================================================================
reCalcCacheChange() ->
    [begin
        ChangeRef = get(?DB_ETS_CHANGE_TAB),
        ets:delete_all_objects(ChangeRef),
        EtsChangeInfo = get(?DB_ETS_CHANGE_INFO_TAB),
        case ets:first(EtsChangeInfo) of
            '$end_of_table' -> ok;
            NowKey ->
                put(?CACHE_LOOP_SAVE_ETS_KEY, NowKey),
                timerLoopSave()
        end
    end || get(?DB_CACHE_DISK_BOOL)].

%%%===================================================================
%% @doc
%%  设置变化量
%% @end
%%%===================================================================
timerLoopSave() ->
    NowKey = get(?CACHE_LOOP_SAVE_ETS_KEY),
    [begin
        EtsChangeInfo = get(?DB_ETS_CHANGE_INFO_TAB),
        NowMSec = c_time:now_millisecond(),
        F = fun
            (_, '$end_of_table') -> {break, '$end_of_table'};
            (_, Key) ->
                modifyChangeTime(NowMSec, [Key], false),
                ets:next(EtsChangeInfo, Key)
        end,
        case c_lib:for(F, NowKey, 0, ?TIME_LOOP_SAVE_ONCE_COUNT) of
            '$end_of_table' ->
                erlang:erase(?CACHE_LOOP_SAVE_ETS_KEY);
            NextKey ->
                erlang:put(?CACHE_LOOP_SAVE_ETS_KEY, NextKey),
                erlang:start_timer(?TIME_LOOP_SAVE_INTERVAL, self(), 'cache_time_loop_save')
        end,
        gen_server:cast('writeSqlMgr', 'writeBySql')
    end || NowKey =/= undefined], ok.

%%%===================================================================
%% @doc
%%  获取清理时间
%% @end
%%%===================================================================
get_clear_time(CatchTInfoRef, NowMSec) ->
    Size = ets:info(CatchTInfoRef, 'size'),
    DiffTime = if
        Size =< 3000 -> 5 * 60 * 1000;
        Size =< 5000 -> 3 * 60 * 1000;
        Size =< 8000 -> 1 * 60 * 1000;
        true -> 30 * 1000
    end,
    NowMSec - DiffTime.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
terminate() ->
    Mod = get(?DB_CACHE_MOD),
    DiskBool = get(?DB_CACHE_DISK_BOOL),
    [begin
        STag = get(?DB_CACHE_TAG),
        KeyTab = get(?DB_ETS_KEY_TAB),
        CacheTab = get(?DB_ETS_CACHE_TAB),
        ChangeInfoTab = get(?DB_ETS_CHANGE_INFO_TAB),
        CacheInfoTab = get(?DB_ETS_CACHE_INFO_TAB),
        write1Sql:terminateWrite(STag, Mod, KeyTab, CacheTab, CacheInfoTab, ChangeInfoTab)
    end || Mod =/= none andalso DiskBool].

%%%===================================================================
%% @doc
%%  修改缓存变化信息
%% @end
%%%===================================================================
update_cache_info(L) ->
    ChangeInfoTab = get(?DB_ETS_CHANGE_INFO_TAB),
    [case ets:lookup(ChangeInfoTab, Key) of
        [#changeInfo{change_ref = ChangeRef}] ->
            ets:delete(ChangeInfoTab, Key);
        _ ->
            [modifyChangeTime(c_time:now_millisecond(), [Key]) || get(?DB_CACHE_DISK_BOOL)]
    end || {Key, ChangeRef} <- L].