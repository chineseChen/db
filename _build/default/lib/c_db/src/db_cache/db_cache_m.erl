%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 17. 9月 2021 15:14
%%%-------------------------------------------------------------------
-module(db_cache_m).
-author("chenkecai").
-include("db_channel.hrl").
-define(DB_CACHE_CHANGE_INDEX, '$db_cache_change_index').

%% API
-export([new_table/0, tab_get/1, tab_get/2, tab_insert/1, tab_del/1, init_tab/0]).
-export([initIndex/0, getIdByIndex/0, makeChangeTabId/0]).
-export([reCalcCacheChange/0]).
-export([doBeforeTerminate/0]).

%%%===================================================================
%% @doc
%%  创建表
%% @end
%%%===================================================================
new_table() ->
    ets:new(?ETS_DB_CACHE_CHANGE, [ordered_set, public, named_table, {keypos, #cacheSupChange.id}]),
    ets:new(?MODULE, [set, public, named_table, {keypos, #cacheMgr.tag}]).

%%%===================================================================
%% @doc
%%  启动child
%% @end
%%%===================================================================
init_tab() ->
    [begin
        L = supervisor:which_children('db_cache_sup'),
        ets:insert(?MODULE, [gen_server:call(element(2, T), 'format_cache_arg') || T <- L])
    end || whereis('db_cache_sup') =/= undefined].

%%%===================================================================
%% @doc
%%  初始index
%% @end
%%%===================================================================
initIndex() ->
    case whereis('db_cache_sup') =/= undefined of
        true ->
            L = supervisor:which_children('db_cache_sup'),
            Index = lists:max([gen_server:call(element(2, T), 'max_change_index') || T <- L]),
            put(?DB_CACHE_CHANGE_INDEX, Index);
        false ->
            put(?DB_CACHE_CHANGE_INDEX, 0)
    end.

%%%===================================================================
%% @doc
%% 制造 变化表的id
%% @end
%%%===================================================================
makeChangeTabId() ->
    gen_server:call(db_cache_mgr, 'make_change_tab_id').

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
getIdByIndex() ->
    NewId = get(?DB_CACHE_CHANGE_INDEX) + 1,
    put(?DB_CACHE_CHANGE_INDEX, NewId).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
tab_insert(Arg) ->
    ets:insert(?MODULE, Arg).

%%%===================================================================
%% @doc
%% 删除信息
%% @end
%%%===================================================================
tab_del(Key) ->
    ets:delete(?MODULE, Key).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
tab_get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] -> none;
        [Value] -> Value
    end.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
tab_get(Key, Pos) ->
    ets:lookup_element(?MODULE, Key, Pos).


%%%===================================================================
%% @doc
%% 重新计算变化量
%% @end
%%%===================================================================
reCalcCacheChange() ->
    ets:delete_all_objects(?ETS_DB_CACHE_CHANGE),
    ets:foldr(fun(#cacheMgr{pid = Pid}, _) ->
        gen_server:cast(Pid, 're_calc_1_cache_change')
    end, [], ?MODULE),
    ok.

%%%===================================================================
%% @doc
%% 所有cache走持久化程序
%% @end
%%%===================================================================
doBeforeTerminate() ->
    ets:delete_all_objects(?ETS_DB_CACHE_CHANGE),
    F = fun(#cacheMgr{pid = Pid}, Acc) -> gen_server:call(Pid, 'do_before_terminate', infinity), Acc end,
    ets:foldl(F, [], ?MODULE),
    ok.
