%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 01. 8月 2021 14:59
%%%-------------------------------------------------------------------
-module(c_sys_lib).
-author("chenkecai").

%% API
-export([start/0, stop/0, clear_db/1, ranch_app_start/0]).

%%--------------------------------------------------------------------
%% @doc
%%  启动函数
%% @end
%%--------------------------------------------------------------------
start() ->
    io:format("os:~p~n", [os:type()]),
    {ok, Root} = init:get_argument(root),
    io:format("root:~p~n", [Root]),
    {ok, Home} = init:get_argument(home),
    io:format("home:~p~n", [Home]),
    io:format("io-encoding:~p~n", [proplists:get_value(encoding, io:getopts(), unknown)]),
    io:format("application start...~n"),
    ok = c_work_path:re_load(?MODULE),
    hot_code:init(),
    {ok, ServerType} = application:get_env(kernel, serverType),
    {AppTags, _} = sysServer:getApps(ServerType),
    {UseTime, _} = timer:tc(fun() -> [ok = application:ensure_started(AppTag, transient) || AppTag <- AppTags] end),
    io:format("application end...~n"),
    io:format("application use ~p ns...~n", [UseTime]),
    io:format("server start ok~n"),
    logger:info("server start ok"),
    ok.

%%%===================================================================
%% @doc
%% 线上服最好等到服务数据全持久化后再停服(查看数据全部持久化后的方法由服务器提供)，保证100%安全
%% @end
%%%===================================================================
stop() ->
    io:format("stop server begin,time:~p~n", [time_lib:second_to_localtime(c_time:now_second())]),
    logger:info("stop server begin..."),
    T1 = erlang:monotonic_time(),
    {ok, ServerType} = application:get_env(kernel, serverType),
    {AppTags, _} = sysServer:getApps(ServerType),
    StopApps = [begin application:stop(AppTag), AppTag end || AppTag <- lists:reverse(AppTags), AppTag =/= c_db, AppTag =/= c_log],
    io:format("~p application stop~n", [StopApps]),
    logger:info("~p application stop", [StopApps]),
    logger:info("stop server running..."),
    spawn_link(fun() ->
        c_lib:while(fun(_) ->
            timer:sleep(2000),
            io:format("stop server running...~n")
        end, none)
    end),
    [begin
        c_db_util:do_before_stop(), %%禁止写入
        application:stop('c_db')
    end || lists:member('c_db', AppTags)],
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
    io:format("stop server end, use time:~pns...~n", [Time]),
    logger:info("stop server end, use time:~pns...~n", [Time]),
    [application:stop('c_log') || lists:member('c_log', AppTags)],
    init:stop(),
    {ok, 'stop'}.

%%%===================================================================
%% @doc
%% 清库
%% @end
%%%===================================================================
clear_db([Node]) ->
    {ok, [L]} = file:consult("./config/app/db.config"),
    case proplists:get_value(c_db, L, none) of
        none ->
            ok;
        SubL ->
            ok = c_work_path:re_load(?MODULE),
            SrcL = [local | proplists:get_value(db_src, SubL, [])],
            {Id, [{_, MysqlCfg} | _]} = proplists:get_value(mysql, SubL, none),
            #{con := ConCfg} = MysqlCfg,
            {ok, Pid} = mysql:start_link([{log_warnings, false} | ConCfg]),
            [NodeSName | _] = string:tokens(atom_to_list(Node), "@"),
            F = fun(DbSrc) ->
                DbNameMaps = proplists:get_value(dbName, SubL, []),
                DbName0 = db_sql:makeDbName(DbSrc, NodeSName, DbNameMaps, Id),
                DbName = list_to_atom(lists:flatten(io_lib:format("`~s`", [DbName0]))),
                Sql = unicode:characters_to_binary(io_lib:format("drop database IF EXISTS ~s;", [DbName])),
                io:format("clear db, running sql (~s)~n", [Sql]),
                ok = mysql:query(Pid, Sql)
            end,
            lists:foreach(F, SrcL),
            mysql:stop(Pid)
    end,
    halt(),
    ok.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================`
ranch_app_start() ->
    ok = application:ensure_started('crypto'),
    ok = application:ensure_started('asn1'),
    ok = application:ensure_started('public_key'),
    ok = application:ensure_started('ssl'),
    ok = application:ensure_started('ranch').

