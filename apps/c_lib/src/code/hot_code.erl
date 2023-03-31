%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 01. 8月 2021 15:22
%%%-------------------------------------------------------------------
-module(hot_code).
-author("chenkecai").

%%%=======================EXPORT=======================
-export([init/0, reload/0, reload_all/0]).
-export([module_md5/1]).
%%-compile(export_all).
%%%=======================INCLUDE======================
-define(DEV_APP_LIST, "c_lib|c_log|test|c_db|c_bi|c_proto|c_data|login|game").
%%%=======================RECORD=======================

%%%=======================DEFINE=======================

%%%=================EXPORTED FUNCTIONS=================

%%%===================================================================
%% @doc
%%  加载所有文件
%% @end
%%%===================================================================
init() ->
    MpStr = get_re_mp(),
    PathL = get_path_l(MpStr),
    ModInfoT = get_all_mod(PathL),
    F = fun(_A, _, {Mod, _}) -> Mod:module_info() end,
    c_lib:tuple_foreach(F, {}, ModInfoT),
    ok.
%% ----------------------------------------------------
%% @doc
%%  加载工作目录
%% @end
%% ----------------------------------------------------
reload() ->
    MpStr = get_re_mp(),
    PathL = get_path_l(MpStr),
    reload_path(PathL),
    ok.

%% ----------------------------------------------------
%% @doc
%%  加载所有目录
%% @end
%% ----------------------------------------------------
reload_all() ->
    MpStr = get_all_re_mp(),
    PathL = get_path_l(MpStr),
    reload_path(PathL),
    ok.

%% ----------------------------------------------------
%% @doc
%%  加载目录下文件
%% @end
%% ----------------------------------------------------
reload_path(PathL) ->
    BeamL = get_all_mod(PathL),
    LoadModL = reload_code(BeamL),
    %%删除已加载的beam
    RemoveModL = remove_code(BeamL),
    if
        LoadModL =/= [] andalso RemoveModL =/= [] ->
            logger:info("更新mod:~p~n移除mod:~p", [LoadModL, RemoveModL]);
        LoadModL =/= [] ->
            logger:info("更新mod:~p", [LoadModL]);
        RemoveModL =/= [] ->
            logger:info("移除mod:~p", [RemoveModL]);
        true ->
            ok
    end.

%%%===================================================================
%% @doc
%%  加载beam
%% @end
%%%===================================================================
reload_code(BeamL) ->
    F = fun(Acc, _, {Mod, _}) ->
        case code:is_loaded(Mod) of
            false ->
                c_lib:get_bool_value(reload_module(Mod), [Mod | Acc], Acc);
            _ ->
                MInfo = Mod:module_info(),
                Object = code:get_object_code(Mod),
                AMd5 = module_md5(MInfo),
                BMd5 = module_md5(Object),
                case AMd5 =/= BMd5 of
                    true -> c_lib:get_bool_value(reload_module(Mod), [Mod | Acc], Acc);
                    false -> Acc
                end
        end
    end,
    c_lib:tuple_foreach(F, [], BeamL).

%%%===================================================================
%% @doc
%%  移除beam
%% @end
%%%===================================================================
remove_code(BeamL) ->
    LoadedL = code:all_loaded(),
    MCStr = get_code_dir() ++ "(?:" ++ ?DEV_APP_LIST ++ ")/ebin/\\w+\\.beam$",
    F = fun({M, Fn}, Acc) ->
        case is_list(Fn) andalso re:run(Fn, MCStr, [global, caseless, {capture, all, list}]) =/= nomatch of
            true ->
                case o_tuple_index:find(M, BeamL) of
                    {null, _} ->
                        code:purge(M),
                        case code:delete(M) of
                            true -> [M | Acc];
                            false -> logger:info("代码移除出错:~p", [M]), Acc
                        end;
                    _Index -> Acc
                end;
            false -> Acc
        end
    end,
    c_lib:foreach(F, [], LoadedL).


%% ----------------------------------------------------
%% @doc
%%  获取所有二进制beam文件
%% @end
%% ----------------------------------------------------
get_all_mod(PathL) ->
    F = fun(Path, Acc) ->
        {ok, FileL} = file:list_dir(Path),
        FF = fun(File, Acc1) ->
            case string:tokens(File, ".") of
                [FileH, "beam"] ->
                    Mod = list_to_atom(FileH),
                    ModFile = filename:join(Path, File),
                    o_tuple:set(Mod, {Mod, ModFile}, Acc1);
                _ -> Acc1
            end
        end,
        c_lib:foreach(FF, Acc, FileL)
    end,
    c_lib:foreach(F, {}, PathL).

%% ----------------------------------------------------
%% @doc
%%  获取path
%% @end
%% ----------------------------------------------------
get_path_l(MpStr) ->
    PathL = code:get_path(),
    {ok, MpBin} = re:compile(MpStr, [caseless]),
    F = fun(Path) ->
        case re:run(Path, MpBin, [global, {capture, all, list}]) of
            {match, _} -> true;
            nomatch -> false
        end
    end,
    lists:filter(F, PathL).

%% ----------------------------------------------------
%% @doc
%%
%% @end
%% ----------------------------------------------------
get_re_mp() ->
    WorkPath = get_code_dir(),
    "^" ++ WorkPath ++ "(?:" ++ ?DEV_APP_LIST ++ ")" ++ "/ebin$".

%% ----------------------------------------------------
%% @doc
%%
%% @end
%% ----------------------------------------------------
get_all_re_mp() ->
    WorkPath = get_code_dir(),
    "^" ++ WorkPath ++ ".*" ++ "/ebin$".

%% ----------------------------------------------------
%% @doc
%%  获取当前目录
%% @end
%% ----------------------------------------------------
get_code_dir() ->
    CodePath = code:which(?MODULE),
    {match, [[LibPath]]} = re:run(CodePath, ".+(?=\\b\\w+/ebin/\\w+\\.beam$)", [global, caseless, {capture, all, list}]),
    LibPath.

%% ----------------------------------------------------
%% @doc
%%
%% @end
%% ----------------------------------------------------
module_md5({M, Beam, _}) ->
    {ok, {M, Vsn}} = beam_lib:md5(Beam),
    Vsn;
module_md5(L) when is_list(L) ->
    {_, Md5} = lists:keyfind(md5, 1, L),
    Md5;
module_md5(Error) ->
    Error.

%% ----------------------------------------------------
%% @doc
%%  加载单个模块
%% @end
%% ----------------------------------------------------
reload_module(Module) ->
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            true;
        {error, Reason} ->
            logger:info("代码加载错误,mode:~p,reason:~p", [Module, Reason]),
            false
    end.

%%%===================LOCAL FUNCTIONS==================

