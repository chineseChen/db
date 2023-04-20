%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%%
%%% @end
%%% Created : 20. 4月 2023 10:23
%%%-------------------------------------------------------------------
-module(dbChan).
-author("ckc").
-include("db_channel.hrl").

%% API
-export([do/1]).

%%%===================================================================
%% @doc
%% 执行
%% @end
%%%===================================================================
do(#channel{type = 'member'} = Channel) ->
    Data = member(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => 'ok', data => Data});
do(#channel{type = 'table_count'} = Channel) ->
    Data = get_table_count(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => 'ok', data => Data});
do(#channel{type = 'get_all_k'} = Channel) ->
    Data = get_all_k(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => 'ok', data => Data});
do(#channel{type = 'get_values'} = Channel) ->
    Data = get_values(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => 'ok', data => Data});
do(#channel{type = 'get', tab_keys = [_ | _]} = Channel) ->
    DataL = get_data(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => ok, data => DataL});
do(#channel{type = 'getKey', tab_keys = [_ | _]} = Channel) ->
    DataL = getKey(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => ok, data => DataL});
do(#channel{type = 'key1st'} = Channel) ->
    Data = key1st(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => ok, data => Data});
do(#channel{type = 'index1st'} = Channel) ->
    Data = index1st(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => ok, data => Data});
do(#channel{type = 'lastKey'} = Channel) ->
    Data = lastKey(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => ok, data => Data});
do(#channel{type = 'lastIndex'} = Channel) ->
    Data = lastIndex(Channel#channel.tab_keys),
    gen_server:reply(Channel#channel.from, #{state => ok, data => Data});
do(Channel) ->
    gen_server:reply(Channel#channel.from, #{state => 'stop', error => 'data_error'}).

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
            {_, Value} = db_channel:get1Data_1(CacheArg, Key, NowMSec),
            {ets:next(LoopTab, Key), [Value | R]}
    end,
    {_, Res} = c_lib:for(F, {ets:first(LoopTab), []}, 0, Num),
    Res.

%%%===================================================================
%% @doc
%%  获取数据
%% @end
%%%===================================================================
get_data(TabKeys) ->
    NowMSec = c_time:now_millisecond(),
    [db_channel:get1Data(STag, Key, NowMSec) || {STag, Key} <- TabKeys].

%%%===================================================================
%% @doc
%% 获取key
%% @end
%%%===================================================================
getKey([{STag, PosIndexL}]) ->
    RefL = db_cache_m:tab_get(STag, #cacheMgr.etsIndex),
    Mod = db_cache_m:tab_get(STag, #cacheMgr.mod),
    Pos = Mod:indexPos(),
    PosL = ?BOOL_VALUE(is_list(Pos), Pos, [Pos]),
    PosRefL = lists:zip(PosL, RefL),
    F = fun({PosVar, Index}) ->
        Ref = proplists:get_value(PosVar, PosRefL, none),
        case ets:lookup(Ref, Index) of
            [] -> none;
            [{_, Value}] -> Value
        end
    end,
    [F(Var) || Var <- PosIndexL].

%%%===================================================================
%% @doc
%% 获取first key
%% @end
%%%===================================================================
key1st(STag) ->
    KeyTab = db_cache_m:tab_get(STag, #cacheMgr.etsKey),
    Key = case KeyTab =/= none of
        true -> ets:first(KeyTab);
        false ->
            CacheTab = db_cache_m:tab_get(STag, #cacheMgr.etsCache),
            ets:first(CacheTab)
    end,
    ?BOOL_VALUE(Key =:= '$end_of_table', none, Key).

%%%===================================================================
%% @doc
%% 获取first index
%% @end
%%%===================================================================
index1st({STag, Index}) ->
    Table = db_channel:getIterateTab(STag, Index),
    case ets:first(Table) of
        '$end_of_table' -> none;
        Var -> Var
    end.

%%%===================================================================
%% @doc
%% 最后的一个key
%% @end
%%%===================================================================
lastKey(STag) ->
    KeyTab = db_cache_m:tab_get(STag, #cacheMgr.etsKey),
    Key = case KeyTab =/= none of
        true -> ets:last(KeyTab);
        false ->
            CacheTab = db_cache_m:tab_get(STag, #cacheMgr.etsCache),
            ets:last(CacheTab)
    end,
    ?BOOL_VALUE(Key =:= '$end_of_table', none, Key).

%%%===================================================================
%% @doc
%% 最后一个index
%% @end
%%%===================================================================
lastIndex({STag, Index}) ->
    Table = db_channel:getIterateTab(STag, Index),
    case ets:last(Table) of
        '$end_of_table' -> none;
        Var -> Var
    end.