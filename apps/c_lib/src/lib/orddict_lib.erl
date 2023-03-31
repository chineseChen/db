%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2022, 
%%% @doc
%%% orddict [{k, v}...]排序列表
%%% 通过内置模块orddict实现orddict类型结构合并
%%% 更新,删除操作通过orddict模块基础函数实现
%%% @end
%%% Created : 31. 5月 2022 11:17
%%%-------------------------------------------------------------------
-module(orddict_lib).
-author("chenkecai").

%% API
-export([merge/1, merge/2]).
-export([merge_i/2, merge_l/1, merge_l/2]).

%%%===================================================================
%% @doc
%% list转排序list
%% @end
%%%===================================================================
merge(L) ->
    orddict:from_list(L).

%%%===================================================================
%% @doc
%% 合并两个list
%% @end
%%%===================================================================
merge(L1, L2) ->
    orddict_merge(fun merge_/3, L1, L2).
merge_(_, V1, V2)
    when (is_integer(V1) orelse is_float(V1)) andalso (is_integer(V2) orelse is_float(V2)) ->
    NV = V1 + V2,
    case NV =/= 0 of
        true -> NV;
        false -> 'delete'
    end;
merge_(_, [_ | _] = V1, [_ | _] = V2) -> V1 ++ V2;
merge_(_, [_ | _] = V1, V2) -> [V2 | V1];
merge_(_, V1, [_ | _] = V2) -> [V1 | V2];
merge_(_, V1, V2) -> [V1, V2].

%%%===================================================================
%% @doc
%%  合并KV V为int类
%% @end
%%%===================================================================
merge_i(L1, L2) ->
    orddict:merge(fun(_, V1, V2) -> V1 + V2 end, L1, L2).

%%%===================================================================
%% @doc
%%  合并KV V为排序唯一list
%% @end
%%%===================================================================
merge_l(L1) ->
    F = fun(NL, {K, V}) -> orddict:append_list(K, lists:usort(V), NL) end,
    z_lib:foreach(F, [], L1).
merge_l(L1, L2) ->
    orddict:merge(fun(_, V1, V2) -> lists:umerge(lists:usort(V1), V2) end, L1, L2).

%%%===================================================================
%% @doc
%%  改变自orddict下的merge,增加F的一个返回,返回delete情况下删除
%% @end
%%%===================================================================
orddict_merge(F, [{K1, _} = E1 | D1], [{K2, _} = E2 | D2]) when K1 < K2 ->
    [E1 | orddict_merge(F, D1, [E2 | D2])];
orddict_merge(F, [{K1, _} = E1 | D1], [{K2, _} = E2 | D2]) when K1 > K2 ->
    [E2 | orddict_merge(F, [E1 | D1], D2)];
orddict_merge(F, [{K1, V1} | D1], [{_K2, V2} | D2]) ->    %K1 == K2
    case F(K1, V1, V2) of
        delete -> orddict_merge(F, D1, D2);
        NV -> [{K1, NV} | orddict_merge(F, D1, D2)]
    end;
orddict_merge(F, [], D2) when is_function(F, 3) -> D2;
orddict_merge(F, D1, []) when is_function(F, 3) -> D1.