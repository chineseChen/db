%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2022, <joinGames>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2022 17:19
%%%-------------------------------------------------------------------
-module(takeSql).
-author("ckc").
-include("db_channel.hrl").
-define(DB_TAKE_CLEAN_KEY_CONTINUE, '$db_take_clean_key_continue').
-define(DB_TAKE_CLEAN_ONCE_LOOP_NUM, 2000).
-define(DB_TAKE_KEY_DOING_TIME_OUT, 60000). %%1分钟未解锁,强解锁

%% API
-export([init/0, write/0, take_1/2, takeKey_2/1, takeMulKey_2/1]).
-export([delLock/1]).
-export([timeOutClean/0]).
-record(takeSql, {
    key = none :: tuple(), %%执行中的key
    pid = none :: pid(), %%获取key的pid
    sTime = 0 :: integer() %%记录插入时间
}).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
write() ->
    Size = ets:info(?ETS_DB_CACHE_CHANGE, size),
    [begin
        L = supervisor:which_children('db_sql_sup'),
        [begin
            Pid = element(2, Value),
            [gen_server:cast(element(2, Value), 'try_write_start') || not ets:member(?DB_WRITE_PROC_STATE_ETS, Pid)]
        end || Value <- L]
    end || is_integer(Size) andalso Size > 0].

%%%===================================================================
%% @doc
%% 初始化
%% @end
%%%===================================================================
init() ->
    ets:new(?MODULE, [ordered_set, public, named_table, {keypos, #takeSql.key}]),
    ets:new(?DB_WRITE_PROC_STATE_ETS, [set, public, named_table, {keypos, 1}]),
    ok.


%%%===================================================================
%% @doc
%% 拿取一个值
%% @end
%%%===================================================================
take_1(ChangeId, Pid) ->
    case ets:lookup(?ETS_DB_CACHE_CHANGE, ChangeId) of
        [] -> '$continue';
        [#cacheSupChange{tag = Tag}] ->
            ChangeTab = db_cache_m:tab_get(Tag, #cacheMgr.etsChange),
            case ets:lookup(ChangeTab, ChangeId) of
                [] ->
                    ets:delete(?ETS_DB_CACHE_CHANGE, ChangeId),
                    '$continue';
                [#cacheChange{key = InfoKey}] ->
                    Mod = db_cache_m:tab_get(Tag, #cacheMgr.mod),
                    case db_data:isMulKey(Mod) of
                        true ->
                            take1MulKey(Tag, InfoKey, ChangeTab, ChangeId, Pid);
                        false ->
                            take1Key(Tag, ChangeTab, ChangeId, InfoKey)
                    end
            end
    end.

%%%%===================================================================
%% @doc
%% 获取复合键
%% @end
%%%===================================================================
take1MulKey(Tag, InfoKey, ChangeTab, ChangeId, Pid) ->
    Key = element(1, InfoKey),
    STabKey = {Tag, Key},
    case ets:member(?MODULE, STabKey) of
        true -> '$continue';
        false ->
            ets:delete(?ETS_DB_CACHE_CHANGE, ChangeId),
            ets:delete(ChangeTab, ChangeId),
            TabKey = {Tag, InfoKey},
            case ets:member(?MODULE, TabKey) of
                false ->
                    ets:insert(?MODULE, #takeSql{key = STabKey, pid = Pid, sTime = c_time:now_millisecond()}),
                    ets:insert(?MODULE, #takeSql{key = TabKey, sTime = c_time:now_millisecond()}),
                    {ok, TabKey};
                true ->
                    '$continue'
            end
    end.

%%%===================================================================
%% @doc
%% 获取一个key
%% @end
%%%===================================================================
take1Key(Tag, ChangeTab, ChangeId, InfoKey) ->
    ets:delete(?ETS_DB_CACHE_CHANGE, ChangeId),
    ets:delete(ChangeTab, ChangeId),
    TabKey = {Tag, InfoKey},
    case ets:member(?MODULE, TabKey) of
        false ->
            ets:insert(?MODULE, #takeSql{key = TabKey, sTime = c_time:now_millisecond()}),
            {ok, TabKey};
        true ->
            '$continue'
    end.


%%%===================================================================
%% @doc
%% 拿取一个值
%% @end
%%%===================================================================
takeKey_2({ChangeTab, Tag, ChangeId}) ->
    case ets:lookup(ChangeTab, ChangeId) of
        [] -> '$continue';
        [#cacheChange{key = InfoKey}] ->
            ets:delete(ChangeTab, ChangeId),
            ets:delete(?ETS_DB_CACHE_CHANGE, ChangeId),
            TabKey = {Tag, InfoKey},
            case ets:member(?MODULE, TabKey) of
                false ->
                    ets:insert(?MODULE, #takeSql{key = TabKey, sTime = c_time:now_millisecond()}),
                    ok;
                true ->
                    '$continue'
            end
    end.

%%%===================================================================
%% @doc
%% 获取复合key
%% @end
%%%===================================================================
takeMulKey_2({ChangeTab, Tag, ChangeId, Pid}) ->
    case ets:lookup(ChangeTab, ChangeId) of
        [] -> '$continue';
        [#cacheChange{key = InfoKey}] ->
            Key = element(1, InfoKey),
            STabKey = {Tag, Key},
            RunF = fun(Bool) ->
                ets:delete(ChangeTab, ChangeId),
                ets:delete(?ETS_DB_CACHE_CHANGE, ChangeId),
                TabKey = {Tag, InfoKey},
                case ets:member(?MODULE, TabKey) of
                    false ->
                        [ets:insert(?MODULE, #takeSql{key = STabKey, pid = Pid, sTime = c_time:now_millisecond()}) || Bool],
                        ets:insert(?MODULE, #takeSql{key = TabKey, sTime = c_time:now_millisecond()}),
                        ok;
                    true ->
                        '$continue'
                end
            end,
            case ets:lookup(?MODULE, STabKey) of
                [] -> RunF(true);
                [#takeSql{pid = Pid}] -> RunF(false);
                _ -> '$continue'
            end
    end.

%%%===================================================================
%% @doc
%% 删除lock
%% @end
%%%===================================================================
delLock([{Tag, _} | _] = TabKeyL) ->
    Mod = db_cache_m:tab_get(Tag, #cacheMgr.mod),
    case db_data:isMulKey(Mod) of
        true ->
            [ets:delete(?MODULE, {Tag, element(1, MulKey)}) || {_, MulKey} <- TabKeyL],
            [ets:delete(?MODULE, TabKey) || TabKey <- TabKeyL];
        false ->
            [ets:delete(?MODULE, TabKey) || TabKey <- TabKeyL]
    end.


%%%===================================================================
%% @doc
%% 定时清理
%% @end
%%%===================================================================
timeOutClean() ->
    Key0 = get(?DB_TAKE_CLEAN_KEY_CONTINUE),
    Key1st = ?BOOL_VALUE(Key0 =/= undefined, Key0, ets:first(?MODULE)),
    [begin
        NowMSec = c_time:now_millisecond(),
        F = fun
            (_, '$end_of_table') -> '$end_of_table';
            (_, TabKey) ->
                case ets:lookup(?MODULE, TabKey) of
                    [#takeSql{sTime = STime, pid = WPid}] when NowMSec >= STime + ?DB_TAKE_KEY_DOING_TIME_OUT ->
                        [begin
                            {Tag, Key} = TabKey,
                            Pid = db_cache_m:tab_get(Tag, #cacheMgr.pid),
                            gen_server:cast(Pid, {'update_cache_info', [{Key, none}]})
                        end || WPid =:= none],
                        ets:delete(?MODULE, TabKey);
                    _ -> ok
                end,
                ets:next(?MODULE, TabKey)
        end,
        CKey = c_lib:for(F, Key1st, 0, ?DB_TAKE_CLEAN_ONCE_LOOP_NUM),
        [put(?DB_TAKE_CLEAN_KEY_CONTINUE, CKey) || CKey =/= '$end_of_table']
    end || Key1st =/= '$end_of_table'],
    ok.
