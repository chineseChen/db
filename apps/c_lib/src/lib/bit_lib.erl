%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 22. 11月 2021 16:09
%%%-------------------------------------------------------------------
-module(bit_lib).
-author("chenkecai").

%% API
-export([set_bit/2, check_bit/2, remove_bit/2, merge_bit/2, reduce_bit/2, to_list/1, map/3, foreach/3]).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
set_bit(Value, Index) ->
    (1 bsl Index) bor Value.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
check_bit(Value, Index) ->
    (1 bsl Index) band Value =/= 0.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
remove_bit(Value, Index) ->
    (bnot (1 bsl Index)) band Value.

%%%===================================================================
%% @doc
%%  合并两个bit值
%% @end
%%%===================================================================
merge_bit(Var1, Var2) ->
    Var1 bor Var2.

%%%===================================================================
%% @doc
%%  var1中删除var2所占位置
%% @end
%%%===================================================================
reduce_bit(Var1, Var2) ->
    (Var1 bxor Var2) band Var1.


%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
to_list(Value) ->
    lists:reverse(map(fun(I, R) -> [I | R] end, [], Value)).


%%%===================================================================
%% @doc
%% bit为1时执行
%% @end
%%%===================================================================
map(F, Acc, Value) ->
    map(F, Acc, Value, 0).
map(_F, Acc, Value, I) when 1 bsl I > Value -> Acc;
map(F, Acc, Value, I) ->
    case check_bit(Value, I) of
        true ->
            case F(I, Acc) of
                {break, NAcc} -> NAcc;
                {ok, NAcc} ->
                    map(F, NAcc, Value, I + 1);
                NAcc ->
                    map(F, NAcc, Value, I + 1)
            end;
        false ->
            map(F, Acc, Value, I + 1)
    end.

%%%===================================================================
%% @doc
%%  遍历
%% @end
%%%===================================================================
foreach(F, Acc, Value) ->
    foreach(F, Acc, Value, 0).
foreach(_F, Acc, Value, I) when 1 bsl I > Value -> Acc;
foreach(F, Acc, Value, I) ->
    case F(I, check_bit(Value, I), Acc) of
        {break, NAcc} -> NAcc;
        {ok, NAcc} ->
            foreach(F, NAcc, Value, I + 1);
        NAcc ->
            foreach(F, NAcc, Value, I + 1)
    end.