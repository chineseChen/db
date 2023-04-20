%%%-------------------------------------------------------------------
%% @doc c_db public API
%% @end
%%%-------------------------------------------------------------------

-module(c_db_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([get_db_src/0]).

start(_StartType, _StartArgs) ->
    c_db_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%% @doc
%% 获取app的源
%% @end
%%%===================================================================
get_db_src() ->
    {ok, DbSrcL} = application:get_env(c_db, db_src),
    {ok, ServerType} = application:get_env(kernel, serverType),
    {_, AppDbL} = sysServer:getApps(ServerType),
    NAppDbSrcL = lists:filter(fun(Src) -> lists:member(Src, AppDbL) end, DbSrcL),
    {ok, [local | NAppDbSrcL]}.
%% internal functions
