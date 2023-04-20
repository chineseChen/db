%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 09. 8月 2021 14:30
%%%-------------------------------------------------------------------
-module(code_lib).
-author("chenkecai").

%% API
-export([decompile/1, dump_viewer/0, compile_load/1]).

%%-------------------------------------------------------------------
%% @doc
%%      反编译
%% @end
%%-------------------------------------------------------------------
-spec decompile(atom()) -> string().
decompile(Mod) ->
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(code:which(Mod), [abstract_code]),
    io_lib:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).

%%%===================================================================
%% @doc
%%  dump
%% @end
%%%===================================================================
dump_viewer() ->
    crashdump_viewer:start().


%%%===================================================================
%% @doc
%%  编译加载需要文件
%% @end
%%%===================================================================
compile_load(L) ->
    F = fun(Filename) ->
        [case compile:file(Filename, [binary]) of
            {ok, Module, Binary} ->
                code:load_binary(Module, Filename, Binary),
                code:purge(Module),
                code:load_file(Module);
            _ -> ok
        end || filelib:is_file(Filename)]
    end,
    lists:foreach(F, L).
