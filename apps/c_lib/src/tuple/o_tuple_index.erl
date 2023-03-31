%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 04. 8月 2021 16:52
%%%-------------------------------------------------------------------
-module(o_tuple_index).
-author("chenkecai").

%% API
-export([find/2, find/3]).

%%%===================================================================
%%% 找到key的位置
%%%===================================================================
find(Key, OTuple) ->
    find(Key, 1, OTuple).
find(Key, KeyPos, OTuple) when is_integer(KeyPos) ->
    Fun = fun(V1, V2) -> compare(V1, KeyPos, V2) end,
    find(Key, Fun, OTuple, 1, size(OTuple));
find(Term, Fun, OTuple) when is_function(Fun) ->
    find(Term, Fun, OTuple, 1, size(OTuple)).


find(_, _, {}, _, _) -> {null, 1};
find(Var, Fun, OTuple, Index, Index) ->
    case Fun(Var, element(Index, OTuple)) of
        'v1>v2' -> {null, Index + 1};
        'v1<v2' -> {null, Index};
        'v1=v2' -> Index
    end;
find(Var, Fun, OTuple, Min, Max) ->
    Index = (Min + Max) div 2,
    case Fun(Var, element(Index, OTuple)) of
        'v1>v2' ->
            find(Var, Fun, OTuple, Index + 1, Max);
        'v1<v2' ->
            find(Var, Fun, OTuple, Min, max(Index - 1, Min));
        'v1=v2' -> Index
    end.

compare(Key, KeyPos, Value) ->
    CompareKey = get_key(KeyPos, Value),
    if
        Key > CompareKey -> 'v1>v2';
        Key < CompareKey -> 'v1<v2';
        Key =:= CompareKey -> 'v1=v2';
        true ->
            %% 2.0 > 2
            case is_float(Key) of
                true -> 'v1>v2';
                false -> 'v1<v2'
            end
    end.

get_key(KeyPos, Tuple) when KeyPos > 0 andalso is_tuple(Tuple) -> element(KeyPos, Tuple);
get_key(_KeyPos, #{key := Value}) -> Value;
get_key(_KeyPos, #{id := Value}) -> Value;
get_key(_, Tuple) -> Tuple.