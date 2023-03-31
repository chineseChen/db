%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 25. 8月 2021 9:58
%%%-------------------------------------------------------------------
-module(db_channel).
-author("chenkecai").
-define(DB_CHANNEL_DATA, 'db_channel_data').
-define(DB_CHANNEL_LOCK_OK, 'db_channel_lock_ok').
-define(DB_CHANNEL_CLEAR_QUEUE, 'db_channel_clear_queue').
-define(DB_CHANNEL_CLEAR_OK, 'db_channel_clear_ok').

-include("db_channel.hrl").
%% API
-export([do/1, do_end/1, handle_time_out/0, delChannel/1, iterate_next/1, iterate_end/1]).
-export([lock_ok/0, unlock/0, unlock/1]).
-export([clearQueue/2, tryClearTab/0, clearQueueReply/0]).
-export([timerTcInit/0, timerTcCalc/3]).

%%%===================================================================
%% @doc
%% 读
%% @end
%%%===================================================================
do(#channel{type = 'member'} = Channel) ->
    Data = member(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => 'ok', data => Data}),
    erlang:exit({'shutdown', 'normal'});
do(#channel{type = 'table_count'} = Channel) ->
    Data = get_table_count(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => 'ok', data => Data}),
    erlang:exit({'shutdown', 'normal'});
do(#channel{type = 'get_all_k'} = Channel) ->
    Data = get_all_k(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => 'ok', data => Data}),
    erlang:exit({'shutdown', 'normal'});
do(#channel{type = 'get_values'} = Channel) ->
    Data = get_values(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => 'ok', data => Data}),
    erlang:exit({'shutdown', 'normal'});
do(#channel{type = 'get', tab_keys = [_ | _]} = Channel) ->
    DataL = get_data(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => ok, data => DataL}),
    erlang:exit({'shutdown', 'normal'});
do(#channel{type = 'iterate'} = Channel) ->
    iterate_init(Channel);
do(#channel{type = 'clear'} = Channel) ->
    put(?DB_CHANNEL_DATA, Channel),
    gen_server:cast('db_lock_proc', {'clear_lock', {Channel#channel.tab_keys, Channel#channel.e_time, self()}});
do(#channel{type = 'replace'} = Channel) ->
    TabKeyL = [TabKey || {TabKey, _} <- Channel#channel.tab_keys],
    put(?DB_CHANNEL_DATA, Channel#channel{tab_keys = TabKeyL, data = Channel#channel.tab_keys}),
    gen_server:cast('db_lock_proc', {'do_lock', {TabKeyL, Channel#channel.s_time, Channel#channel.e_time, self()}});
do(#channel{type = Type, tab_keys = [_ | _]} = Channel)
    when Type =:= 'update' orelse Type =:= 'handle' ->
    put(?DB_CHANNEL_DATA, Channel),
    gen_server:cast('db_lock_proc', {'do_lock', {Channel#channel.tab_keys, Channel#channel.s_time, Channel#channel.e_time, self()}});
do(Channel) ->
    gen_server:reply(Channel#channel.from, #{state => 'stop', error => 'data_error'}),
    erlang:exit({'shutdown', 'normal'}).

%%%===================================================================
%% @doc
%% 获取数据
%% @end
%%%===================================================================
lock_ok() ->
    Channel = get(?DB_CHANNEL_DATA),
    case Channel#channel.type of
        'clear' ->
            LockTab = db_cache_m:tab_get(Channel#channel.tab_keys, #cacheMgr.etsLock),
            ets:insert(LockTab, #lock_run{pid = self(), eTime = Channel#channel.e_time}),
            clear_tab(Channel);
        _ ->
            [begin
                LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLock),
                ets:insert(LockTab, #lock_run{pid = self(), eTime = Channel#channel.e_time})
            end || {STag, _} <- Channel#channel.tab_keys],
            case Channel#channel.type of
                'replace' ->
                    DataL = get_data(Channel#channel.tab_keys),
                    do_write_1(Channel#channel.data, [], Channel#channel{data = DataL});
                _ ->
                    DataL = get_data(Channel#channel.tab_keys),
                    put(?DB_CHANNEL_DATA, Channel#channel{data = DataL}),
                    gen_server:reply(Channel#channel.from, #{state => 'ok', data => DataL, pid => self()})
            end
    end.

%%%===================================================================
%% @doc
%%  获取数据
%% @end
%%%===================================================================
get_data(TabKeys) ->
    NowMSec = c_time:now_millisecond(),
    [get1Data(STag, Key, NowMSec) || {STag, Key} <- TabKeys].

%%%===================================================================
%% @doc
%% 获取单个数据
%% @end
%%%===================================================================
get1Data(STag, Key, NowMSec) ->
    CacheArg = db_cache_m:tab_get(STag),
    get1Data_1(CacheArg, Key, NowMSec).
get1Data_1(CacheArg, Key, NowMSec) ->
    %%先找事务数据
    case ets:lookup(CacheArg#cacheMgr.etsCacheInfo, Key) of
        [#cacheInfo{running = #running{data = Var}}] ->
            {{CacheArg#cacheMgr.tag, Key}, Var};
        [#cacheInfo{mulCount = MulCount}] ->
            get1Data_2(CacheArg, Key, MulCount, NowMSec);
        [] ->
            get1Data_2(CacheArg, Key, 0, NowMSec)
    end.
get1Data_2(CacheArg, Key, MulCount, NowMSec) ->
    case ets:lookup(CacheArg#cacheMgr.etsKey, Key) of
        [] -> {{CacheArg#cacheMgr.tag, Key}, none};
        _ ->
            Mod = CacheArg#cacheMgr.mod,
            ValueL = db_data:getMulValue(Mod, CacheArg#cacheMgr.etsCache, Key),
            case CacheArg#cacheMgr.diskBool of
                true ->
                    ModBool = db_data:isMulKey(Mod),
                    case ValueL =:= [] orelse (ModBool andalso length(ValueL) =/= MulCount) of
                        true ->
                            get1Data_3(CacheArg, Key, NowMSec);
                        false ->
                            Var = ?BOOL_VALUE(db_data:isMulKey(Mod), ValueL, hd(ValueL)),
                            gen_server:cast(CacheArg#cacheMgr.pid, {'insert', {Key, NowMSec}}),
                            {{CacheArg#cacheMgr.tag, Key}, Var}
                    end;
                false ->
                    Var = ?BOOL_VALUE(ValueL =:= [], none,
                        ?BOOL_VALUE(db_data:isMulKey(Mod), ValueL, hd(ValueL))),
                    {{CacheArg#cacheMgr.tag, Key}, Var}
            end
    end.
get1Data_3(CacheArg, Key, NowMSec) ->
    STag = CacheArg#cacheMgr.tag,
    case db_sql_data:get_data(STag, Key) of
        none ->
            {{STag, Key}, none};
        Data0 ->
            Mod = CacheArg#cacheMgr.mod,
            Data = case db_data:isMulKey(Mod) of
                true ->
                    L = lists:keysort(1, [{db_data:getMulKey(Mod, Var), Var} || Var <- Data0]),
                    gen_server:cast(CacheArg#cacheMgr.pid, {'insert', {Key, L, NowMSec}}),
                    [Var || {_, Var} <- L];
                false ->
                    Data1 = ?BOOL_VALUE(Data0 =:= [], none, hd(Data0)),
                    gen_server:cast(CacheArg#cacheMgr.pid, {'insert', {Key, Data1, NowMSec}}),
                    Data1
            end,
            {{STag, Key}, Data}
    end.


%%%===================================================================
%% @doc
%% 写
%% @end
%%%===================================================================
do_end(#{from := From} = Maps) ->
    Channel = get(?DB_CHANNEL_DATA),
    DataL = maps:get('data', Maps, []),
    IDataL = maps:get('insert_data', Maps, []),
    case DataL =/= [] orelse IDataL =/= [] of
        true ->
            put(?DB_CHANNEL_DATA, Channel#channel{insert_data = IDataL, from = From}),
            do_write_1(DataL, IDataL, Channel#channel{from = From});
        false ->
            gen_server:reply(From, #{state => 'ok'}),
            free_channel(Channel)
    end.

%%%===================================================================
%% @doc
%% 写
%% @end
%%%===================================================================
do_write_1(DataL, IDataL, Channel) ->
    #channel{data = ODataL} = Channel,
    case analysis_data(ODataL, DataL) of
        PidMsgL1 when is_list(PidMsgL1) ->
            IDataL1 = form_insert_data(IDataL),
            do_write_2(Channel, PidMsgL1, IDataL1, ODataL);
        {error, Reason} ->
            gen_server:reply(Channel#channel.from, #{state => 'stop', error => 'data_error', reason => Reason}),
            free_channel(Channel)
    end.

%%%===================================================================
%% @doc
%% 写
%% @end
%%%===================================================================
do_write_2(Channel, PidMsgL1, IDataL1, ODataL) ->
    case c_lib:foreach(fun doInsertData/2, true, IDataL1) of
        true ->
            [ok = gen_server:call(Pid, {'write_start', {self(), Msg}}, 1000) || {_, Pid, Msg} <- PidMsgL1],
            case checkLockTime(Channel#channel.tab_keys) of
                true ->
                    [begin
                        Arg = {Pid, {'write_end', {self(), element(1, Msg)}}},
                        c_lib:for(fun doWriteEnd/2, Arg, 1, 3)
                    end || {_, Pid, Msg} <- PidMsgL1],
                    [begin
                        Arg = {Pid, {'write_end', {self(), element(1, Msg)}}},
                        c_lib:for(fun doWriteEnd/2, Arg, 1, 3)
                    end || {_, Pid, Msg} <- IDataL1],
                    Reply = ?BOOL_VALUE(Channel#channel.info =:= 'old_data_flag', #{state => 'ok', data => ODataL}, #{state => 'ok'}),
                    gen_server:reply(Channel#channel.from, Reply);
                false ->
                    gen_server:reply(Channel#channel.from, #{state => 'stop', error => 'data_error', reason => 'time_out'}),
                    roll_back(Channel)
            end;
        Reason ->
            gen_server:reply(Channel#channel.from, #{state => 'stop', error => 'data_error', reason => Reason}),
            roll_back(Channel)
    end,
    makeChangeFlag(PidMsgL1, IDataL1),
    free_channel(Channel),
    ok.

%%%===================================================================
%% @doc
%% 打上变化标记
%% @end
%%%===================================================================
makeChangeFlag(PidMsgL1, IDataL1) ->
    NowMSec = c_time:now_millisecond(),
    F = fun(STag, ChangeL) ->
        [begin
            ChangeTab = db_cache_m:tab_get(STag, #cacheMgr.etsChange),
            db_cache:modifyChangeTime(NowMSec, STag, ChangeTab, ChangeL)
        end || db_cache_m:tab_get(STag, #cacheMgr.diskBool)]
    end,
    [F(STag, element(4, Msg)) || {STag, _, Msg} <- PidMsgL1],
    [F(STag, element(3, Msg)) || {STag, _, Msg} <- IDataL1],
    ok.

%%%===================================================================
%% @doc
%% 插入新数据
%% @end
%%%===================================================================
doInsertData({_, Pid, Msg}, InsertR) ->
    case gen_server:call(Pid, {'insert_data', {self(), Msg}}, 1000) of
        ok -> InsertR;
        {error, Reason} -> {break, Reason}
    end.

%%%===================================================================
%% @doc
%% 通知结束
%% @end
%%%===================================================================
doWriteEnd(_, {Pid, Msg} = R) ->
    case catch gen_server:call(Pid, Msg, 1000) of
        ok -> {break, ok};
        _ -> R
    end.

%%%%===================================================================
%% @doc
%% 检查锁是否存及容错时间
%% 容错时间后服务器自动解锁
%% @end
%%%===================================================================
checkLockTime(TableKeys) ->
    SelfPid = self(),
    CheckF = fun(TabKey, _) ->
        case ets:lookup('db_lock', TabKey) of
            %%同一个channel下endTime一样
            [#lock{pid = SelfPid, e_time = ETime}] -> ETime;
            _ -> {break, false}
        end
    end,
    case c_lib:foreach(CheckF, 0, TableKeys) of
        false -> false;
        MinTime ->
            MinTime - c_time:now_millisecond() > db_lock:getExtraEndTime()
    end.

%%%===================================================================
%% @doc
%%  释放channel
%% @end
%%%===================================================================
free_channel(Channel) ->
    lists:foreach(fun({STag, _}) ->
        LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLock),
        ets:delete(LockTab, self())
    end, Channel#channel.tab_keys),
    gen_server:cast('db_lock_proc', {'delete_channel', {Channel#channel.tab_keys, self(), Channel#channel.s_time}}),
    erlang:exit({'shutdown', 'normal'}).

%%%===================================================================
%% @doc
%%  退出channel
%% @end
%%%===================================================================
delChannel(Reason) ->
    Channel = get(?DB_CHANNEL_DATA),
    [begin
        Type = Channel#channel.type,
        if
            Type =:= 'replace' orelse Type =:= 'update' orelse Type =:= 'handle' ->
                roll_back(Channel),
                lists:foreach(fun({STag, _}) ->
                    LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLock),
                    ets:delete(LockTab, self())
                end, Channel#channel.tab_keys),
                gen_server:cast('db_lock_proc', {'delete_channel', {Channel#channel.tab_keys, self(), Channel#channel.s_time}});
            Type =:= 'clear' ->
                LockTab = db_cache_m:tab_get(Channel#channel.tab_keys, #cacheMgr.etsLock),
                ets:delete(LockTab, self()),
                gen_server:cast('db_lock_proc', {'delClearLock', {Channel#channel.tab_keys, self()}});
            true ->
                ok
        end,
        gen_server:reply(Channel#channel.from, #{state => 'stop', error => Reason})
    end || Channel =/= undefined].

%%%===================================================================
%% @doc
%% 回滚
%% @end
%%%===================================================================
roll_back(Channel) ->
    F = fun({{STag, Key}, _}) ->
        Pid = db_cache_m:tab_get(STag, #cacheMgr.pid),
        gen_server:cast(Pid, {'roll_back', {self(), Key}})
    end,
    [F(KV) || KV <- Channel#channel.data],
    [F(KV) || KV <- Channel#channel.insert_data],
    ok.


%%%===================================================================
%% @doc
%%  分析数据
%% @end
%%%===================================================================
analysis_data(ODataL, DataL) ->
    F = fun
        ({{STag, Key} = SKey, Value}, R) ->
            case lists:keyfind(SKey, 1, ODataL) of
                false -> {break, {error, "data key error"}};
                {_, Value} -> R;
                {_, OdValue} ->
                    CachePid = db_cache_m:tab_get(STag, #cacheMgr.pid),
                    Mod = db_cache_m:tab_get(STag, #cacheMgr.mod),
                    case db_data:isMulKey(Mod) of
                        true ->
                            case db_data:analyse2Value(Mod, OdValue, Value) of
                                'no_change' -> R;
                                {ok, ChangeL} ->
                                    NValue = ?BOOL_VALUE(Value =:= 'delete', Value,
                                        [{db_data:getMulKey(Mod, Var), Var} || Var <- Value]),
                                    [{STag, CachePid, {Key, NValue, OdValue, ChangeL}} | R]
                            end;
                        false ->
                            [{STag, CachePid, {Key, Value, OdValue, [Key]}} | R]
                    end
            end;
        (_, _) -> {break, {error, "data form error"}}
    end,
    c_lib:foreach(F, [], DataL).

%%%===================================================================
%% @doc
%% 分析插入数据
%% @end
%%%===================================================================
form_insert_data(IDataL) ->
    [begin
        Pid = db_cache_m:tab_get(STag, #cacheMgr.pid),
        Mod = db_cache_m:tab_get(STag, #cacheMgr.mod),
        case db_data:isMulKey(Mod) of
            true ->
                ChangeL = [db_data:getMulKey(Mod, Var) || Var <- Value],
                NValue = ?BOOL_VALUE(Value =:= 'delete', Value,
                    [{db_data:getMulKey(Mod, Var), Var} || Var <- Value]),
                {STag, Pid, {Key, NValue, ChangeL}};
            false ->
                {STag, Pid, {Key, Value, [Key]}}
        end
    end || {{STag, Key}, Value} <- IDataL].

%%%===================================================================
%% @doc
%%  遍历的下一个key
%% @end
%%%===================================================================
iterate_init(Channel) ->
    #channel{tab_keys = STag, from = From, timer_ref = TimerRef, time_out = TimeOut, info = Info} = Channel,
    Table = case db_cache_m:tab_get(STag, #cacheMgr.etsKey) of
        none -> db_cache_m:tab_get(STag, #cacheMgr.etsCache);
        KeyTab -> KeyTab
    end,
    SortType = proplists:get_value('sortType', Info, 'ascending'),
    {Key, NextF} = case SortType of
        'ascending' -> {proplists:get_value('key', Info, ets:first(Table)),
            fun(FKey) -> ets:next(Table, FKey) end};
        _ -> {proplists:get_value('key', Info, ets:last(Table)),
            fun(FKey) -> ets:prev(Table, FKey) end}
    end,
    put('iterate_arg', {NextF, Key, TimerRef, TimeOut}),
    gen_server:reply(From, #{state => 'ok', pid => self(), key => Key}),
    [erlang:exit({'shutdown', 'normal'}) || Key =:= '$end_of_table'].
iterate_next(From) ->
    case get('iterate_arg') of
        {NextF, Key, TimerRef, TimeOut} ->
            NextKey = NextF(Key),
            case db_channel_proc:re_set_timer(TimerRef, TimeOut) of
                false -> erlang:erase('iterate_arg');
                NTimerRef -> put('iterate_arg', {NextF, NextKey, NTimerRef, TimeOut})
            end,
            gen_server:reply(From, #{state => 'ok', pid => self(), key => NextKey}),
            [erlang:exit({'shutdown', 'normal'}) || NextKey =:= '$end_of_table'];
        'undefined' ->
            erlang:exit({'shutdown', 'iterate_next_error'})
    end.

%%%===================================================================
%% @doc
%%  遍历结束
%% @end
%%%===================================================================
iterate_end(From) ->
    gen_server:reply(From, #{state => ok}),
    erlang:exit({'shutdown', 'normal'}).

%%%===================================================================
%% @doc
%% 尝试清表
%% @end
%%%===================================================================
tryClearTab() ->
    case get(?DB_CHANNEL_DATA) of
        #channel{tab_keys = STag, type = 'clear'} = Channel ->
            LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLock),
            [clear_tab(Channel) || db_lock:checkClear(LockTab)];
        _ -> ok
    end.

%%%===================================================================
%% @doc
%% 清理队列
%% @end
%%%===================================================================
clearQueue(Pid, ETime) ->
    case get(?DB_CHANNEL_CLEAR_OK) of
        true -> gen_server:cast(Pid, 'clearQueueReply');
        _ ->
            case get(?DB_CHANNEL_CLEAR_QUEUE) of
                undefined -> put(?DB_CHANNEL_CLEAR_QUEUE, [{Pid, ETime}]);
                L -> put(?DB_CHANNEL_CLEAR_QUEUE, lists:ukeysort(1, [{Pid, ETime} | L]))
            end
    end.

%%%===================================================================
%% @doc
%%  清空表
%% @end
%%%===================================================================
clear_tab(Channel) ->
    #channel{tab_keys = {Src, _} = STag, from = From} = Channel,
    CachePid = db_cache_m:tab_get(STag, #cacheMgr.pid),
%%    T1 = erlang:monotonic_time(),
    gen_server:call(CachePid, {'clear_data', Src}, Channel#channel.time_out),
%%    T2 = erlang:monotonic_time(),
%%    Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
%%    io:format("mod:~p line:~p ==============arg:~p~n", [?MODULE, ?LINE, Time]),
    freeClearQueue(),
    put(?DB_CHANNEL_CLEAR_OK, true),
    gen_server:reply(From, #{state => 'ok'}),
    LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLock),
    ets:delete(LockTab, self()),
    gen_server:cast('db_lock_proc', {'delClearLock', {Channel#channel.tab_keys, self()}}),
    ok.

%%%===================================================================
%% @doc
%% 释放清理队列
%% @end
%%%===================================================================
freeClearQueue() ->
    L = erlang:erase(?DB_CHANNEL_CLEAR_QUEUE),
    [begin
        NowMSec = c_time:now_millisecond(),
        [gen_server:cast(Pid, 'clearQueueReply') || {Pid, ETime} <- L, NowMSec < ETime]
    end || is_list(L)].

%%%===================================================================
%% @doc
%% 清理队列返回
%% @end
%%%===================================================================
clearQueueReply() ->
    case get(?DB_CHANNEL_DATA) of
        #channel{e_time = ETime, from = From, type = 'clear'} ->
            [begin
                NowMSec = c_time:now_millisecond(),
                [gen_server:reply(From, #{state => 'ok'}) || NowMSec < ETime],
                freeClearQueue(),
                put(?DB_CHANNEL_CLEAR_OK, true),
                erlang:exit({'shutdown', 'normal'})
            end || get(?DB_CHANNEL_CLEAR_OK) =:= undefined];
        _ ->
            ok
    end.

%%%===================================================================
%% @doc
%% 请求超时
%% @end
%%%===================================================================
handle_time_out() ->
    Channel = get(?DB_CHANNEL_DATA),
    [gen_server:reply(Channel#channel.from, #{state => 'stop', error => 'handle_time_out', reason => "handle time out"}) || Channel =/= undefined].


%%%===================================================================
%% @doc
%%  解锁LockKey
%% @end
%%%===================================================================
unlock() ->
    unlock(none).
unlock(From) ->
    case get(?DB_CHANNEL_LOCK_OK) of
        undefined ->
            Channel = get(?DB_CHANNEL_DATA),
            TabKeys = Channel#channel.tab_keys,
            Bool = gen_server:call('db_lock_proc', {'queue_doing', {TabKeys, Channel#channel.s_time, Channel#channel.e_time, self()}}),
            [gen_server:reply(From, Bool) || From =/= none],
            [begin
                put(?DB_CHANNEL_LOCK_OK, true),
                lists:foreach(fun({STag, _}) ->
                    LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLockQueue),
                    ets:delete(LockTab, {Channel#channel.e_time, self()})
                end, TabKeys)
            end || Bool],
            ok;
        _ ->
            [gen_server:reply(From, false) || From =/= none],
            ok
    end.

%%%===================================================================
%% @doc
%% 获取数据总数
%% @end
%%%===================================================================
get_table_count(STag) ->
    KeyTab = db_cache_m:tab_get(STag, #cacheMgr.etsKey),
    Size = case KeyTab =/= none of
        true ->
            ets:info(KeyTab, size);
        false ->
            CacheTab = db_cache_m:tab_get(STag, #cacheMgr.etsCache),
            ets:info(CacheTab, size)
    end,
    ?BOOL_VALUE(Size =:= undefined, 0, Size).

%%%===================================================================
%% @doc
%% 获取数据总数
%% @end
%%%===================================================================
member({STag, Key}) ->
    KeyTab = db_cache_m:tab_get(STag, #cacheMgr.etsKey),
    case KeyTab =/= none of
        true -> ets:member(KeyTab, Key);
        false ->
            CacheTab = db_cache_m:tab_get(STag, #cacheMgr.etsCache),
            ets:member(CacheTab, Key)
    end.

%%%===================================================================
%% @doc
%% 获取所有k
%% @end
%%%===================================================================
get_all_k(STag) ->
    KeyTab = db_cache_m:tab_get(STag, #cacheMgr.etsKey),
    case KeyTab =/= none of
        true -> ets:select(KeyTab, [{{'$1'}, [], ['$1']}]);
        false ->
            CacheTab = db_cache_m:tab_get(STag, #cacheMgr.etsCache),
            KeyPos = ets:info(CacheTab, keypos),
            ets:select(CacheTab, [{'$1', [], [{element, KeyPos, '$1'}]}])
    end.

%%%===================================================================
%% @doc
%% 获取前Num个数据
%% @end
%%%===================================================================
get_values({STag, Num}) ->
    NowMSec = c_time:now_millisecond(),
    CacheArg = db_cache_m:tab_get(STag),
    LoopTab =
        case CacheArg#cacheMgr.etsKey =:= none of
            true -> CacheArg#cacheMgr.etsCache;
            false -> CacheArg#cacheMgr.etsKey
        end,
    F = fun
        (_, {'$end_of_table', R}) -> {'$end_of_table', R};
        (_, {Key, R}) ->
            {_, Value} = get1Data_1(CacheArg, Key, NowMSec),
            {ets:next(LoopTab, Key), [Value | R]}
    end,
    {_, Res} = c_lib:for(F, {ets:first(LoopTab), []}, 0, Num),
    Res.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
timerTcInit() ->
    put(?DB_TIMER_TC_MSEC, erlang:monotonic_time()).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
timerTcCalc(Mod, Line, V) ->
    DefTimerTc0 = erlang:monotonic_time(),
    DefTimerTc = erlang:convert_time_unit(DefTimerTc0 - get(?DB_TIMER_TC_MSEC), native, microsecond),
    [logger:debug("~p ~p ~p start ~p", [self(), Mod, Line, DefTimerTc]) || DefTimerTc > V],
    put(?DB_TIMER_TC_MSEC, DefTimerTc0),
    ok.