%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 17. 10月 2021 17:11
%%%-------------------------------------------------------------------
-module(rand_lib).
-author("chenkecai").

%% API
-export([between/2]).
-export([rand_w/0, rand_w/1, rand_w/2]).
-export([rand_num_s/3]).

%%%===================================================================
%% @doc
%%  Bottom, Top之间随机
%% @end
%%%===================================================================
between(Bottom, Top) when Bottom > Top ->
    between(Top, Bottom);
between(Bottom, Top) ->
    Diff = Top - Bottom,
    Rand = rand:uniform(Diff + 1),
    Bottom + Rand - 1.

%% ----------------------------------------------------
%% @doc
%%  index1 -index2 随机数
%% @end
%% ----------------------------------------------------
rand_num_s(Index1, Index2, Count) when Index1 > Index2 ->
    rand_num_s(Index2, Index1, Count);
rand_num_s(Bottom, Top, Count) when Count < 1000 ->
    LenCount = Top - Bottom + 1,
    RealCount =
        case LenCount > Count of
            true -> Count;
            false -> LenCount
        end,
    F = fun(Nth, Acc) ->
        RandVar = rand:uniform(LenCount - Nth + 1),
        [{Nth, RandVar} | Acc]
    end,
    RandL = c_lib:for(F, [], 1, RealCount),
    Result = rand_num_s_(RandL, LenCount),
    [Bottom + V - 1 || {_, V} <- lists:keysort(1, Result)].

rand_num_s_(RandL, Top) ->
    SortL = lists:keysort(2, RandL),
    rand_num_s_(SortL, Top, 0, []).

rand_num_s_([], _LenCount, _LVar, R) ->
    R;
rand_num_s_([{K, Var} = KV | T], Top, LastVar, R) ->
    case Var > LastVar of
        true ->
            case Var > Top of
                true ->
                    rand_num_s_(T, Top, Var, [{K, Var rem Top} | R]);
                false ->
                    rand_num_s_(T, Top, Var, [KV | R])
            end;
        false ->
            rand_num_s_([{K, LastVar + 1} | T], Top, LastVar, R)
    end.

%%%===================================================================
%% @doc
%% 随机字母数字_
%% @end
%%%===================================================================
rand_w() ->
    Initial = rand:uniform(62) - 1,
    rand_w(<<Initial>>, 7).
rand_w(Rem) ->
    rand_w(<<>>, Rem).

rand_w(Bin, 0) ->
    Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_">>,
    <<<<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin>>;
rand_w(Bin, Rem) ->
    Next = rand:uniform(62) - 1,
    rand_w(<<Bin/binary, Next>>, Rem - 1).