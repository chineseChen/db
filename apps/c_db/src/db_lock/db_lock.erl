%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 25. 8月 2021 15:18
%%%-------------------------------------------------------------------
-module(db_lock).
-author("chenkecai").
-include("db_channel.hrl").

%% API
-export([getExtraEndTime/0]).
-export([clearLock/1, checkClear/1]).
-export([new_table/0, doing/1, del_channel/1]).
-export([delClearChannel/1]).

%%%===================================================================
%% @doc
%%  创建表
%% @end
%%%===================================================================
new_table() ->
    ets:new(?MODULE, [ordered_set, protected, named_table, {keypos, #lock.key}]).

%%%===================================================================
%% @doc
%% 清理锁,可插队
%% @end
%%%===================================================================
clearLock({STag, ETime, Pid}) ->
    LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLock),
    Bool = checkClear(LockTab), %%清表锁库，库没有清完成，禁止修改
    RunPid = case ets:lookup(?MODULE, STag) of
        [#lock{pid = OPid, e_time = OldTime}] ->
            case ETime > OldTime of
                true ->
                    gen_server:cast(Pid, {clearPidQueue, OPid, OldTime}),
                    ets:insert(?MODULE, #lock{key = STag, pid = Pid, e_time = ETime + ?LOCK_END_EXTRA_TIME}),
                    Pid;
                false ->
                    gen_server:cast(OPid, {clearPidQueue, Pid, ETime}),
                    OPid
            end;
        [] ->
            ets:insert(?MODULE, #lock{key = STag, pid = Pid, e_time = ETime + ?LOCK_END_EXTRA_TIME}),
            Pid
    end,
    [gen_server:cast(RunPid, 'lock_ok') || Bool],
    Bool.

%%%===================================================================
%% @doc
%% 获取额外的容错时间
%% @end
%%%===================================================================
getExtraEndTime() ->
    ?LOCK_END_EXTRA_TIME.

%%%===================================================================
%% @doc
%% 检查是否可以清理
%% @end
%%%===================================================================
checkClear(LockTab) ->
    NowMSec = c_time:now_millisecond(),
    ets:safe_fixtable(LockTab, true),
    First = ets:first(LockTab),
    try
        checkClear_(First, NowMSec, LockTab)
    after
        ets:safe_fixtable(LockTab, false)
    end.
checkClear_('$end_of_table', _, _) -> true;
checkClear_(Key, NowMSec, LockTab) ->
    case ets:lookup(LockTab, Key) of
        [#lock_run{eTime = ETime}] when NowMSec < ETime -> false;
        [_] ->
            ets:delete(LockTab, Key),
            checkClear_(ets:next(LockTab, Key), NowMSec, LockTab);
        [] ->
            checkClear_(ets:next(LockTab, Key), NowMSec, LockTab)
    end.

%%%===================================================================
%% @doc
%% 执行锁,加排队
%% @end
%%%===================================================================
doing({TabKeyL, STime, ETime, Pid}) ->
    NowMSec = c_time:now_millisecond(),
    case analysis(TabKeyL, Pid, NowMSec) of
        {true, Locks, _} ->
            do_lock(Locks, ETime, Pid), true;
        {false, _, Locks} ->
            lock_fail(Locks, Pid, STime, ETime), false
    end.

%%%===================================================================
%% @doc
%% 分析tab_keys
%% @end
%%%===================================================================
analysis(TabKeyL, Pid, NowMSec) ->
    F = fun({STag, _} = TabKey, {_, L1, L2} = R) ->
        case ets:lookup(?MODULE, STag) of %%有没有锁表
            [] -> analysis_1(TabKey, Pid, NowMSec, R);
            [Lock] ->
                case NowMSec >= Lock#lock.e_time of
                    true ->
                        ets:delete(?MODULE, STag),
                        analysis_1(TabKey, Pid, NowMSec, R);
                    false ->
                        {false, L1, [TabKey | L2]}
                end
        end
    end,
    c_lib:foreach(F, {true, [], []}, TabKeyL).

%%%===================================================================
%% @doc
%% 分析单条数据
%% @end
%%%===================================================================
analysis_1(TabKey, Pid, NowMSec, {Bool, L1, L2}) ->
    case ets:lookup(?MODULE, TabKey) of %%锁单条数据
        [] -> {Bool, [#lock{key = TabKey} | L1], L2};
        [#lock{pid = Pid}] -> {break, {false, [], []}}; %%
        [Lock] ->
            case NowMSec >= Lock#lock.e_time of
                true ->
                    ets:delete(?MODULE, TabKey),
                    {Bool, [#lock{key = TabKey} | L1], L2};
                false -> {false, L1, [TabKey | L2]}
            end
    end.

%%%===================================================================
%% @doc
%% 上锁
%% @end
%%%===================================================================
do_lock(LockL, ETime, Pid) ->
    [ets:insert(?MODULE, Lock#lock{pid = Pid, e_time = ETime + ?LOCK_END_EXTRA_TIME}) || Lock <- LockL],
    gen_server:cast(Pid, 'lock_ok').

%%%===================================================================
%% @doc
%% 上锁失败
%% @end
%%%===================================================================
lock_fail(TabKeys, Pid, ChannelSTime, ETime) ->
    F = fun({STag, Key}) ->
        LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLockQueue),
        ets:insert(LockTab, #lock_queue{uid = {Key, ChannelSTime, Pid}, eTime = ETime})
    end,
    lists:foreach(F, TabKeys).

%%%===================================================================
%% @doc
%%  删除channel, 刷新link
%% @end
%%%===================================================================
del_channel({TabKeyL, Pid, STime}) ->
    F = fun(TabKey) ->
        {STag, Key} = TabKey,
        LockTab = db_cache_m:tab_get(STag, #cacheMgr.etsLockQueue),
        case ets:lookup(?MODULE, TabKey) of
            [#lock{pid = Pid}] ->
                ets:delete(?MODULE, TabKey),
                {true, {TabKey, LockTab}};
            _ ->
                ets:delete(LockTab, {Key, STime, Pid}),
                false
        end
    end,
    FreeL = lists:filtermap(F, TabKeyL),
    gen_server:cast('dbLockQueueProc', {free_lock_queue, FreeL}).

%%%===================================================================
%% @doc
%% 删除清理表数据channel
%% @end
%%%===================================================================
delClearChannel({STag, RunPid}) ->
    case ets:lookup(?MODULE, STag) of
        [#lock{key = STag, pid = Pid}] ->
            ets:delete(?MODULE, STag),
            [gen_server:cast(Pid, 'clearQueueReply') || Pid =/= RunPid];
        _ -> ok
    end,
    gen_server:cast('dbLockQueueProc', {free_clear, STag}),
    gen_server:cast(RunPid, 'clear_ok').
