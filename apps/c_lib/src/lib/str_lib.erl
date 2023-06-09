%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 09. 8月 2021 17:41
%%%-------------------------------------------------------------------
-module(str_lib).
-author("chenkecai").

%% API
-export([str_2_term/1, term_2_str/1, term_2_str1/1, is_string/1]).


%%%===================================================================
%% @doc
%% term序列化
%% @end
%%%===================================================================
term_2_str(Term) ->
    lists:flatten(io_lib:format("~0p", [Term])).

%%%===================================================================
%% @doc
%% term序列化,会自动加入\n
%% @end
%%%===================================================================
term_2_str1(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

%%%===================================================================
%% @doc
%% 序列化str转化为term
%% @end
%%%===================================================================
str_2_term(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
is_string(Str) when is_list(Str) ->
    lists:all(fun(Char) -> is_integer(Char) andalso Char =< 16#ff end, Str);
is_string(_) -> false.
