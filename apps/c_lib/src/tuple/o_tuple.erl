%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 04. 8月 2021 17:36
%%%-------------------------------------------------------------------
-module(o_tuple).
-author("chenkecai").

%% API
-export([get/2, set/3]).

%%%===================================================================
%%% 获取key值
%%%===================================================================
get(Key, OTuple) ->
    case o_tuple_index:find(Key, OTuple) of
        {null, _} -> false;
        Index -> element(Index, OTuple)
    end.

%%%===================================================================
%%% 设置key-value
%%%===================================================================
set(Key, Value, OTuple) ->
    case o_tuple_index:find(Key, OTuple) of
        {null, Index} -> erlang:insert_element(Index, OTuple, Value);
        Index -> erlang:setelement(Index, OTuple, Value)
    end.