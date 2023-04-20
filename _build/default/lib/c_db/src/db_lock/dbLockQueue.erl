%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%%
%%% @end
%%% Created : 28. 2月 2023 11:23
%%%-------------------------------------------------------------------
-module(dbLockQueue).
-author("ckc").
-include("db_channel.hrl").

%% API
-export([freeQueue/1, freeAllQueue/1]).

%%%===================================================================
%% @doc
%% 释放队列,以key为单位
%% @end
%%%===================================================================
freeQueue(FreeL) ->
    NowMSec = c_time:now_millisecond(),
    [freeQueue(TabKey, Tab, NowMSec) || {TabKey, Tab} <- FreeL].
freeQueue({STag, Key} = TabKey, Tab, NowMSec) ->
    case ets:lookup(db_lock, STag) of
        [#lock{e_time = ETime, pid = Pid}] when NowMSec < ETime ->
            gen_server:cast(Pid, 'tryClearTab');
        _ ->
            Key1st = ets:next(Tab, {Key, 0, none}),
            freeQueue_(Key1st, TabKey, NowMSec, Tab)
    end.

%%%===================================================================
%% @doc
%% 循环
%% 释放队列,以key为单位
%% @end
%%%===================================================================
freeQueue_('$end_of_table', _, _, _) -> ok;
freeQueue_({Key1, _, _}, {_, Key2}, _, _) when Key1 =/= Key2 -> ok;
freeQueue_({_, _, Pid} = Key, TabKey, NowMSec, Tab) ->
    case ets:lookup(Tab, Key) of
        [#lock_queue{eTime = ETime}] when NowMSec < ETime ->
            case catch gen_server:call(Pid, 'unlock', 1000) of
                true -> ets:delete(Tab, Key);
                false ->
                    freeQueue_(ets:next(Tab, Key), TabKey, NowMSec, Tab);
                _ ->
                    case ets:member('db_lock', TabKey) of
                        true -> ok;
                        false ->
                            ets:delete(Tab, Key),
                            freeQueue_(ets:next(Tab, Key), TabKey, NowMSec, Tab)
                    end
            end;
        _ ->
            ets:delete(Tab, Key),
            freeQueue_(ets:next(Tab, Key), TabKey, NowMSec, Tab)
    end.

%%%===================================================================
%% @doc
%% 尝试释放所有排队
%% @end
%%%===================================================================
freeAllQueue(STag) ->
    NowMSec = c_time:now_millisecond(),
    case ets:lookup(db_lock, STag) of
        [#lock{e_time = ETime, pid = Pid}] when NowMSec < ETime ->
            gen_server:cast(Pid, 'tryClearTab');
        _ ->
            Tab = db_cache_m:tab_get(STag, #cacheMgr.etsLockQueue),
            freeAllQueue_(ets:first(Tab), NowMSec, none, Tab)
    end.
freeAllQueue_('$end_of_table', _, _, _) -> ok;
freeAllQueue_({Key, _, Pid} = Uid, NowMSec, LastLockKey, Tab) ->
    case ets:lookup(Tab, Uid) of
        [#lock_queue{eTime = ETime}] when NowMSec < ETime ->
            case Key =/= LastLockKey of
                true ->
                    case catch gen_server:call(Pid, 'unlock', 1000) of
                        true ->
                            ets:delete(Tab, Uid),
                            freeAllQueue_(ets:next(Tab, Uid), NowMSec, Key, Tab);
                        _ ->
                            freeAllQueue_(ets:next(Tab, Uid), NowMSec, LastLockKey, Tab)
                    end;
                false ->
                    freeAllQueue_(ets:next(Tab, Uid), NowMSec, LastLockKey, Tab)
            end;
        _ ->
            ets:delete(Tab, Uid),
            freeAllQueue_(ets:next(Tab, Uid), NowMSec, LastLockKey, Tab)
    end.
