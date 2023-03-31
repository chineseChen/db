%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 23. 8月 2021 15:55
%%%-------------------------------------------------------------------
-module(db_sql).
-author("chenkecai").

-include("db_channel.hrl").

%% API
-export([init/0, ping/0]).
-export([getReadPid/0, getReadIndex/0, getDbName/1]).
-export([makeDbName/4]).
%%%===================================================================
%% @doc
%%  创建sql进程
%% @end
%%%===================================================================
init() ->
    {ok, DbSrcL} = c_db_app:get_db_src(),
    {ok, {Id, CfgL}} = application:get_env(mysql),
    {_, Cfg} = hd(CfgL),
    DbNameMaps = application:get_env(c_db, dbName, []),
    initSqlDb(DbSrcL, Id, Cfg, DbNameMaps),
    L = supervisor:which_children(db_sql_sup),
    create_sql_proc(CfgL, L),
    ok.

%%%===================================================================
%% @doc
%%  创建数据库
%% @end
%%%===================================================================
initSqlDb(DbSrc, Id, Arg, DbNameMaps) when is_atom(DbSrc) ->
    initSqlDb([DbSrc], Id, Arg, DbNameMaps);
initSqlDb(DbSrcL, Id, #{con := Cfg}, DbNameMaps) ->
    {ok, Pid} = mysql:start_link(Cfg),
    CreateDbStr = "CREATE DATABASE IF NOT EXISTS ~s DEFAULT CHARSET utf8 COLLATE utf8_general_ci;use ~s;",
    [NodeSName | _] = string:tokens(atom_to_list(node()), "@"),
    db_extra_sql:readAlterTabSql(),
    F = fun(DbSrc) ->
        DbName1 = makeDbName(DbSrc, NodeSName, DbNameMaps, Id),
        DbName = list_to_atom(lists:flatten(io_lib:format("`~s`", [DbName1]))),
        put({'db_src_tag', DbSrc}, DbName),
        SqlBin = unicode:characters_to_binary(io_lib:format(CreateDbStr, [DbName, DbName])),
        ok = mysql:query(Pid, unicode:characters_to_binary(SqlBin)),
        DbNameStr = lists:flatten(io_lib:format("'~s'", [DbName1])),
        ShowTabStr = unicode:characters_to_binary(io_lib:format("select table_name from `information_schema`.`tables` where table_schema=~s;", [DbNameStr])),
        {ok, _, TableL0} = mysql:query(Pid, ShowTabStr),
        TableL = [string:to_lower(binary_to_list(Var)) || [Var] <- TableL0],
        db_extra_sql:dealAlterTab(Pid, DbName, TableL),
        dealTab(Pid, DbSrc, DbName, TableL),
        ok
    end,
    [F(DbSrcVar) || DbSrcVar <- DbSrcL, DbSrcVar =/= 'sys'],
    db_extra_sql:eraseAlterInfo(),
    ok = mysql:stop(Pid),
    ok.

%%%===================================================================
%% @doc
%% 处理普通表
%% @end
%%%===================================================================
dealTab(Pid, DbSrc, DbName, TableL) ->
    [begin
        CreateF = fun
            (_Acc, _, {Tag, Mod}) when is_atom(Mod) ->
                Bool = Mod:save_type() =:= 'disk' andalso lists:member(DbSrc, Mod:db_src()),
                [begin
                    TagBin = string:lowercase(atom_to_list(Tag)),
                    case lists:member(TagBin, TableL) of
                        false ->
                            case catch mysql:query(Pid, Mod:create()) of
                                ok -> ok;
                                MysqlError ->
                                    error({error, Tag, Mod:create(), MysqlError})
                            end;
                        true ->
                            db_extra_sql:alterTab(Pid, DbName, Tag, Mod),
                            ok
                    end
                end || Bool];
            (_Acc, _, _) -> ok
        end,
        c_lib:tuple_foreach(CreateF, [], dbT:get_mod())
    end || erlang:function_exported(dbT, get_mod, 0)], ok.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
create_sql_proc([], _) -> ok;
create_sql_proc([{write, Cfg} | T], L) ->
    #{con := ConCfg, pool_size := Size} = Cfg,
    F = fun(I, _) ->
        case lists:keymember({write, I}, 1, L) of
            false -> db_sql_sup:start_child({write, I}, 'writeSqlProc', 'writeSqlProc', ConCfg);
            _ -> ok
        end
    end,
    c_lib:for(F, [], 1, Size + 1),
    create_sql_proc(T, L);
create_sql_proc([{read, Cfg} | T], L) ->
    #{con := ConCfg, pool_size := Size} = Cfg,
    F = fun(I, _) ->
        case lists:keymember({write, I}, 1, L) of
            false -> db_sql_sup:start_child({read, I}, 'mysql', 'mysql_conn', ConCfg);
            _ -> ok
        end
    end,
    c_lib:for(F, [], 1, Size + 1),
    put(read_size, Size),
    put(read_index, 1),
    create_sql_proc(T, L).


%%%===================================================================
%% @doc
%% ping read mysql
%% @end
%%%===================================================================
ping() ->
    [Pid ! ping || {_, Pid, _, _} <- supervisor:which_children(db_sql_sup)],
    ok.

%%%===================================================================
%% @doc
%%  获取读index
%% @end
%%%===================================================================
getReadPid() ->
    Index = gen_server:call('db_sql_mgr', 'get_read_index'),
    L = supervisor:which_children('db_sql_sup'),
    {_, Pid, _, _} = lists:keyfind({read, Index}, 1, L),
    Pid.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
getReadIndex() ->
    Count = get(read_size),
    Index = get(read_index),
    NIndex = c_lib:get_loop_index(Index, Count),
    put(read_index, NIndex + 1),
    NIndex.

%%%===================================================================
%% @doc
%%  获取数据库名
%% @end
%%%===================================================================
getDbName(DbSrc) ->
    case get({'db_src_tag', DbSrc}) of
        undefined -> false;
        Var -> Var
    end.

%%%===================================================================
%% @doc
%% 获取db_name
%% @end
%%%===================================================================
makeDbName(DbSrc, NodeSName, DbNameMaps, Id) ->
    case proplists:get_value(DbSrc, DbNameMaps, none) of
        none ->
            case Id of
                0 -> io_lib:format("~s_~s", [NodeSName, DbSrc]);
                _ -> io_lib:format("~s_~w_~s", [NodeSName, Id, DbSrc])
            end;
        DbName0 -> atom_to_list(DbName0)
    end.