%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_sql_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/4]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.

%%%===================================================================
%% @doc
%%  启动子进程
%% @end
%%%===================================================================
start_child(Id, Mod, CallBackMod, ConCfg) ->
    AChild = #{id => Id,
        start => {Mod, start_link, [ConCfg]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [CallBackMod]},
    {ok, _} = supervisor:start_child(?MODULE, AChild).
