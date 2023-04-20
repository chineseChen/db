%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 08. 8月 2021 11:48
%%%-------------------------------------------------------------------
-module(c_lib).
-author("chenkecai").

%% API
-export([get_bool_value/3]).
-export([while/2, for/4, foreach/3, tuple_foreach/3]).
-export([perms/1, md5_hex/1]).
-export([get_loop_index/2]).
-export([sort/2, sort/3, sort_/2]).
-export([checks/3]).

-spec while(function(), any()) -> any().
while(F, Args) ->
    case F(Args) of
        {ok, A} -> while(F, A);
        {break, A} -> A;
        A -> while(F, A)
    end.

-spec for(function(), any(), integer(),
        integer()) -> any().
for(F, Args, I, N) when is_integer(N) andalso I < N -> for1(F, Args, I, N);
for(F, Args, I, N) when is_integer(N) andalso I > N -> for2(F, Args, I, N);
for(_F, Args, N, N) -> Args.

for1(F, Args, I, N) when I < N ->
    case F(I, Args) of
        {ok, A} -> for1(F, A, I + 1, N);
        {break, A} -> A;
        A -> for1(F, A, I + 1, N)
    end;
for1(_F, Args, _, _) -> Args.

for2(F, Args, I, N) when I > N ->
    case F(I, Args) of
        {ok, A} -> for2(F, A, I - 1, N);
        {break, A} -> A;
        A -> for2(F, A, I - 1, N)
    end;
for2(_F, Args, _, _) -> Args.

-spec foreach(function(), any(), list()) -> any().
foreach(F, Args, [H | T]) ->
    case F(H, Args) of
        {break, A} -> A;
        {ok, A} -> foreach(F, A, T);
        A -> foreach(F, A, T)
    end;
foreach(_F, Args, []) -> Args.

-spec tuple_foreach(function(), any(), tuple()) -> any().
tuple_foreach(F, A, Tuple) ->
    tuple_foreach(Tuple, tuple_size(Tuple), F, A).

tuple_foreach(Tuple, N, F, A) when N > 0 ->
    case F(A, N, element(N, Tuple)) of
        {ok, A1} -> tuple_foreach(Tuple, N - 1, F, A1);
        {break, A1} -> A1;
        A1 -> tuple_foreach(Tuple, N - 1, F, A1)
    end;
tuple_foreach(_Tuple, _N, _F, A) -> A.

%%%===================================================================
%% @doc
%%  Bool?Var1:Var2
%% @end
%%%===================================================================
get_bool_value(true, F, _) when is_function(F) -> F();
get_bool_value(false, _, F) when is_function(F) -> F();
get_bool_value(true, Var, _) -> Var;
get_bool_value(false, _, Var) -> Var.

%%%===================================================================
%% @doc
%% 回文异构
%% [1, 2, 3]所有组合
%% @end
%%%===================================================================
perms([]) -> [[]];
perms(L) ->
    [[Var | T] || Var <- L, T <- perms(L -- [Var])].

%%%===================================================================
%% @doc
%%  md5
%% @end
%%%===================================================================
md5_hex(S) ->
    Md5Bin = erlang:md5(S),
    <<<<(int_to_hex(N))/binary>> || <<N>> <= Md5Bin>>.

int_to_hex(N) when N < 256 ->
    A = hex(N div 16),
    B = hex(N rem 16),
    <<A, B>>.

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).

%%%===================================================================
%% @doc
%%  获循环中的序数
%% @end
%%%===================================================================
get_loop_index(Count, LoopNum) when Count > 0 andalso LoopNum > 0 ->
    get_loop_index_(Count, LoopNum).

get_loop_index_(0, LoopNum) -> LoopNum;
get_loop_index_(Count, LoopNum) when Count =< LoopNum -> Count;
get_loop_index_(Count, LoopNum) ->
    get_loop_index_(Count rem LoopNum, LoopNum).

%%%===================================================================
%% @doc
%%  能用lists:sort用list:sort
%%  插入
%% @end
%%%===================================================================
sort([], _, V) -> [V];
sort([H | T], F, V) ->
    case F(H, V) of
        true -> [H | sort(T, F, V)];
        false -> [V, H | T]
    end.

%%%===================================================================
%% @doc
%%  能用lists:sort用list:sort
%%  插入,升序
%% @end
%%%===================================================================
sort_([], V) -> [V];
sort_([{_, A1} = H | T], {_, A2} = V) when A1 > A2 ->
    [V, H | T];
sort_([H | T], V) ->
    [H | sort_(T, V)].

%%%===================================================================
%% @doc
%%  能用lists:sort用list:sort
%%  插入,升序
%% @end
%%%===================================================================
sort([], V) -> [V];
sort([H | T], V) when H > V ->
    [V, H | T];
sort([H | T], V) ->
    [H | sort(T, V)].

%%%===================================================================
%% @doc
%% 检查
%% @end
%%%===================================================================
checks(ConL, Args, RunF) ->
    F = fun(H, _) ->
        case RunF(H, Args) of
            true -> true;
            Error -> Error
        end
    end,
    c_lib:foreach(F, true, ConL).


