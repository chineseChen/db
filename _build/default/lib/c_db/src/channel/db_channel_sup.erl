%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_channel_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = #{id => 'db_channel_proc',
        start => {'db_channel_proc', start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => ['db_channel_proc']},
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => simple_one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%% @doc
%% 开启一个channel
%% @end
%%%===================================================================
start_child() ->
    supervisor:start_child(?MODULE, []).