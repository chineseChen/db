%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2022, joinGame
%%% @doc
%%% 系统表,辅助功能相关
%%% @end
%%% Created : 15. 8月 2022 16:03
%%%-------------------------------------------------------------------
-module(db_extra_sql).
-author("chenkecai").
-include("db_channel.hrl").

%% API
-export([dealAlterTab/3, eraseAlterInfo/0, readAlterTabSql/0, alterTab/4]).
-export([clearWriteErrorTab/0]).

%%%===================================================================
%% @doc
%% 创建修改alter表信息
%% @end
%%%===================================================================
dealAlterTab(Pid, DbName, TableL) ->
    T = dbT:get_mod(),
    F = fun(_, _, {Tag, Mod}) ->
        [begin
            TagBin = string:lowercase(atom_to_list(Tag)),
            case lists:member(TagBin, TableL) of
                true ->
                    alterTab(Pid, DbName, Tag, Mod);
                false ->
                    ok = mysql:query(Pid, Mod:create())
            end, ok
        end || is_atom(Mod) andalso Mod:save_type() =:= 'disk' andalso lists:member(sys, Mod:db_src())]
    end,
    c_lib:tuple_foreach(F, [], T),
    ok.

%%%===================================================================
%% @doc
%% 读取修改信息
%% @end
%%%===================================================================
readAlterTabSql() ->
    case application:get_env(kernel, server_version) of
        {ok, {V1, V2, V3} = Version} ->
            SqlFile = lists:flatten(io_lib:format("./config/db/main/sql/db_~p_~p_~p.sql", [V1, V2, V3])),
            case file:read_file(SqlFile) of
                {ok, AlterSql} ->
                    put('alter_sql_info', {Version, AlterSql}), ok;
                _ -> ok
            end;
        _ -> ok
    end.

%%%===================================================================
%% @doc
%%  清理表
%% @end
%%%===================================================================
eraseAlterInfo() ->
    erase('alter_sql_info').

%%%===================================================================
%% @doc
%% 增加,删除字段
%% @end
%%%===================================================================
alterTab(Pid, DbName, Tag, Mod) ->
    alterTabFromFile(Pid, DbName, Tag),
    alterTabByComparing(Pid, DbName, Tag, Mod),
    ok.

%%%===================================================================
%% @doc
%% 从文件中提取sql语句修改
%% @end
%%%===================================================================
alterTabFromFile(Pid, DbName, Tag) ->
    case get('alter_sql_info') of
        {Version, AlterSql} ->
            Data = getAlterTab(Pid, DbName, Tag),
            [begin
                Str = io_lib:format("^(?<!--|--)\\s*alter[\\w\\s]*(?:|~s\\.|`~s`\\.)(?:~s|`~s`)[^;]+;", [DbName, DbName, Tag, Tag]),
                {ok, Re} = re:compile(Str, [unicode, caseless, multiline]),
                case re:run(AlterSql, Re, [global, {capture, all, binary}]) of
                    {match, L} ->
                        AlterSqlBin = list_to_binary(L),
                        case catch mysql:query(Pid, AlterSqlBin) of
                            ok -> ok;
                            MySqlError -> error({error, AlterSqlBin, MySqlError})
                        end,
                        NData0 = dbT_dbAlterTab:set_version(Data, Version),
                        NData = dbT_dbAlterTab:set_time(NData0, c_time:now_second()),
                        {Str0, ValueL, SetVarStr} = db_sql_data:form('dbT_dbAlterTab', NData, all),
                        UpSqlStr = io_lib:format("INSERT INTO ~s.`~s` ~s ON DUPLICATE KEY UPDATE ~s;", [DbName, 'dbAlterTab', Str0, SetVarStr]),
                        ok = mysql:query(Pid, unicode:characters_to_binary(UpSqlStr), ValueL);
                    _ -> ok
                end
            end || dbT_dbAlterTab:get_version(Data) =/= Version];
        _ -> ok
    end.
%%%===================================================================
%% @doc
%% 获取表中值
%% @end
%%%===================================================================
getAlterTab(Pid, DbName, Key) ->
    Sql = dbT_dbAlterTab:select(DbName),
    Index = dbT_dbAlterTab:key_index(),
    {_, ErlType, _, _} = dbT_dbAlterTab:info_by_index(Index),
    NKey = db_sql_data:sql_value_by_type(atom_to_binary(Key), ErlType),
    {ok, ColumnT, DataL} = mysql:query(Pid, Sql, [NKey]),
    case DataL of
        [] -> dbT_dbAlterTab:init(atom_to_binary(Key));
        _ ->
            [Res] = db_sql_data:analyse('dbT_dbAlterTab', ColumnT, DataL), Res
    end.

%%%===================================================================
%% @doc
%% 仅增加字段其它复杂alter,请用alter修改
%% @end
%%%===================================================================
alterTabByComparing(Pid, DbName, Tag, Mod) ->
    Rec = Mod:init(),
    Sql = io_lib:format("DESC ~s.`~s`;", [DbName, Tag]),
    {ok, _, L} = mysql:query(Pid, Sql),
    ColumnL = [string:to_lower(lists:flatten(io_lib:format("`~s`", [Column]))) || [Column | _] <- L],
    F = fun(Acc, I, Value) ->
        case Mod:info_by_index(I) of
            none -> Acc;
            Info ->
                ColumnStr = string:to_lower(binary_to_list(element(1, Info))),
                case lists:member(ColumnStr, ColumnL) of
                    true -> Acc;
                    false ->
                        {Column, EType, SqlType, Desc} = Info,
                        AfterStr = case I of
                            2 -> "FIRST";
                            _ ->
                                case Mod:info_by_index(I - 1) of
                                    none -> "";
                                    PreInfo -> io_lib:format("AFTER ~s", [element(1, PreInfo)])
                                end
                        end,
                        SqlTypeStr = c_db_util:format_sql_type(SqlType),
                        DefaultStr = c_db_util:format_column_default(EType, SqlType, Value),
                        AlterSql = lists:flatten(io_lib:format("ALTER TABLE `~s` ADD COLUMN ~s ~s~ts COMMENT '~ts' ~s;", [
                            Tag, Column, SqlTypeStr, DefaultStr, Desc, AfterStr
                        ])),
                        [AlterSql | Acc]
                end
        end
    end,
    AlterSqlL = c_lib:tuple_foreach(F, [], Rec),
    [begin
        AlterSqlStr = unicode:characters_to_binary(lists:flatten(AlterSqlL)),
        ok = mysql:query(Pid, AlterSqlStr), ok
    end || AlterSqlL =/= []],
    ok.

%%%===================================================================
%% @doc
%% 创建一个mysql连接
%% @end
%%%===================================================================
doingByMysqlCon(F) ->
    {ok, {_, [{_, #{con := Cfg}} | _]}} = application:get_env('c_db', 'mysql'),
    {ok, Pid} = mysql:start_link(Cfg),
    Res = F(Pid),
    ok = mysql:stop(Pid),
    {ok, Res}.

%%%===================================================================
%% @doc
%% 清理写错误表
%% @end
%%%===================================================================
clearWriteErrorTab() ->
    clearTab(?DB_WRITE_ERROR_TAB_TAG).

%%%===================================================================
%% @doc
%% 直接清表仅适用没有缓的表(比如sys类型的表),
%% 数据丢失将无法找回
%% 慎用慎用慎用慎用慎用,
%% 慎用慎用慎用慎用慎用,
%% 慎用慎用慎用慎用慎用,
%% 数据丢失将无法找回
%% @end
%%%===================================================================
clearTab(Tab) ->
    doingByMysqlCon(fun(Pid) ->
        {ok, SrcL} = c_db_app:get_db_src(),
        F = fun(DBSrc) ->
            DBName = gen_server:call('db_sql_mgr', {'get_db_name', DBSrc}),
            Str = io_lib:format("TRUNCATE TABLE ~s.`~s`;", [DBName, Tab]),
            mysql:query(Pid, unicode:characters_to_binary(Str))
        end,
        lists:map(F, SrcL)
    end).