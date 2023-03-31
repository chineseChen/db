%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%%
%%% @end
%%% Created : 13. 3月 2023 16:41
%%%-------------------------------------------------------------------
-module(db_data).
-author("ckc").
-include("db_channel.hrl").

%% API
-export([analyse2Value/3, getMulKey/2]).
-export([isMulKey/1, isNoChangeData/3]).
-export([getMulValue/3, delMulData/3]).
-export([formatMulKV/2]).

%===================================================================
%% @doc
%% 分析复合键值
%% @end
%%%===================================================================
analyse2Value(Mod, Old, New) ->
    case Old =:= none orelse Old =:= [] of
        true ->
            case New =:= 'delete' orelse New =:= [] of
                true -> 'no_change';
                false ->
                    {ok, [getMulKey(Mod, Value) || Value <- New]}
            end;
        false ->
            case New =:= 'delete' orelse New =:= [] of
                true -> {ok, [getMulKey(Mod, Value) || Value <- Old]};
                false ->
                    L1 = [{getMulKey(Mod, Value), Value} || Value <- Old],
                    L2 = lists:keysort(1, [{getMulKey(Mod, Value), Value} || Value <- New]),
                    case compare(L1, L2) of
                        [] -> 'no_change';
                        ChangeL -> {ok, ChangeL}
                    end
            end
    end.

%%%===================================================================
%% @doc
%% 是否为复合key
%% @end
%%%===================================================================
isMulKey(none) -> false;
isMulKey(Mod) ->
    is_tuple(Mod:key_index()).

%%%===================================================================
%% @doc
%% 获取复合key
%% @end
%%%===================================================================
getMulKey(Mod, Value) ->
    KeyIndex = Mod:key_index(),
    F = fun(Acc, I, Index) ->
        erlang:setelement(I, Acc, element(Index, Value))
    end,
    c_lib:tuple_foreach(F, KeyIndex, KeyIndex).

%%%===================================================================
%% @doc
%% 获取多
%% @end
%%%===================================================================
getMulValue(Mod, CatchTab, Key) ->
    case isMulKey(Mod) of
        true ->
            KeyIndex = Mod:key_index(),
            Key1st = erlang:setelement(1, KeyIndex, Key),
            CenterL = ets:lookup(CatchTab, Key1st),
            PreKey1st = ets:prev(CatchTab, Key1st),
            PreL = forPreMulValue(PreKey1st, Key, CatchTab, fun(PreKey, Tab, R) ->
                [V || {_, V} <- ets:lookup(Tab, PreKey)] ++ R
            end, [V || {_, V} <- CenterL]),
            NextKey1st = ets:next(CatchTab, Key1st),
            PreL ++ forNextMulValue(NextKey1st, Key, CatchTab, fun(NextKey, Tab, R) ->
                [V || {_, V} <- ets:lookup(Tab, NextKey)] ++ R
            end, []);
        false -> ets:lookup(CatchTab, Key)
    end.

%%%===================================================================
%% @doc
%% 删除复合数据
%% @end
%%%===================================================================
delMulData(Mod, CatchTab, Key) ->
    KeyIndex = Mod:key_index(),
    Key1st = erlang:setelement(1, KeyIndex, Key),
    ets:delete(CatchTab, Key1st),
    PreKey1st = ets:prev(CatchTab, Key1st),
    forPreMulValue(PreKey1st, Key, CatchTab, fun(PreKey, Tab, _) ->
        ets:delete(Tab, PreKey)
    end, []),
    NextKey1st = ets:next(CatchTab, Key1st),
    forNextMulValue(NextKey1st, Key, CatchTab, fun(NextKey, Tab, _) ->
        ets:delete(Tab, NextKey)
    end, []),
    ok.


%%%===================================================================
%% @doc
%% 向前遍历找key
%% @end
%%%===================================================================
forPreMulValue('$end_of_table', _, _, _, R) -> R;
forPreMulValue(MulKey, Key, _CatchTab, _F, R) when element(1, MulKey) =/= Key -> R;
forPreMulValue(MulKey, Key, CatchTab, F, R) ->
    NR = F(MulKey, CatchTab, R),
    forPreMulValue(ets:prev(CatchTab, MulKey), Key, CatchTab, F, NR).

%%%===================================================================
%% @doc
%% 向后便利
%% @end
%%%===================================================================
forNextMulValue('$end_of_table', _, _, _, R) -> R;
forNextMulValue(MulKey, Key, _CacheTab, _F, R) when element(1, MulKey) =/= Key -> R;
forNextMulValue(MulKey, Key, CacheTab, F, R) ->
    F(MulKey, CacheTab, forNextMulValue(ets:next(CacheTab, MulKey), Key, CacheTab, F, R)).

%%%===================================================================
%% @doc
%% 构造k-v
%% @end
%%%===================================================================
formatMulKV(KeyIndex, ValueL) ->
    F = fun(Var) ->
        FF = fun(I, Acc) ->
            MulKey = erlang:setelement(I, Acc, element(element(I, KeyIndex), Var)),
            {MulKey, Var}
        end,
        c_lib:for(FF, KeyIndex, 1, size(KeyIndex) + 1)
    end,
    [F(Value) || Value <- ValueL].

%%%===================================================================
%% @doc
%% 是否有变化
%% @end
%%%===================================================================
isNoChangeData(Mod, ChangeTab, Key) ->
    KeyIndex = Mod:key_index(),
    MulKey = erlang:setelement(1, KeyIndex, Key),
    ets:next(ChangeTab, MulKey) =:= '$end_of_table' andalso ets:prev(ChangeTab, MulKey) =:= '$end_of_table'.

%%%===================================================================
%% @doc
%% 对比变化
%% @end
%%%===================================================================
compare(L1, L2) ->
    compare(L1, L2, []).
compare([], [], Acc) -> Acc;
compare([], L2, Acc) ->
    F = fun({Key, _}, Acc0) -> [Key | Acc0] end,
    lists:foldl(F, Acc, L2);
compare(L1, [], Acc) ->
    F = fun({Key, _}, Acc0) -> [Key | Acc0] end,
    lists:foldl(F, Acc, L1);
compare([Var | L1], [Var | L2], Acc) ->
    compare(L1, L2, Acc);
compare([{K, _} | L1], [{K, _} | L2], Acc) ->
    compare(L1, L2, [K | Acc]);
compare([{K1, _} | L1], [{K2, _} | _] = L2, Acc) when K1 < K2 ->
    compare(L1, L2, [K1 | Acc]);
compare([{K1, _} | _] = L1, [{K2, _} | L2], Acc) when K1 > K2 ->
    compare(L1, L2, [K2 | Acc]).
