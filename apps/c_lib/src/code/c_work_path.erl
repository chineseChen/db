%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 01. 8月 2021 15:26
%%%-------------------------------------------------------------------
-module(c_work_path).
-author("chenkecai").

%% API
-export([re_load/1]).
%%-complie(export_all).

%%--------------------------------------------------------------------
%% @doc
%%  传入启动函数?MODULE
%% @end
%%--------------------------------------------------------------------
re_load(Mod) ->
    ModPath = code:which(Mod),
    del_path(ModPath),
    load_path(ModPath),
    ok.

%%%===================================================================
%% @doc
%%  删除启动文件路径
%% @end
%%%===================================================================
del_path(ModPath) ->
    {ok, NowPath} = file:get_cwd(),
    {match, [[WorkPath]]} = re:run(ModPath, "(?<=^" ++ NowPath ++ "/)" ++ ".+(?=/.+/ebin/.+\\.beam$)", [global, caseless, {capture, all, list}]),
    PathL = code:get_path(),
    F = fun(PathVar, _) ->
        case re:run(PathVar, "^" ++ WorkPath ++ ".+", [global, caseless, {capture, all, list}]) of
            {match, [[PathVar]]} -> code:del_path(PathVar), {break, ok};
            _ -> ok
        end
    end,
    c_lib:foreach(F, ok, PathL),
    ok.

%%%===================================================================
%% @doc
%%  加载文件
%% @end
%%%===================================================================
load_path(ModPath) ->
    {match, [[SupPath]]} = re:run(ModPath, ".+(?=/\\w+/\\w+/ebin/\\w+.beam$)", [global, caseless, {capture, all, list}]),
    SubPathL = filelib:wildcard("lib/*/ebin", SupPath),
    PathL = [filename:join(SupPath, SubPath) || SubPath <- SubPathL],
    code:add_pathsa(PathL),
    ok.
