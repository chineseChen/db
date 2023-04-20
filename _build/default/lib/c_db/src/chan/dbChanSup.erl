%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(dbChanSup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([startChild/0, getPid/1]).
-define(DB_CHAN_NUM, 'chan_num').

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = #{id => 'dbChanProc',
        start => {'dbChanProc', start_link, []},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => ['dbChanProc']},
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => simple_one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [AChild]}}.

%%%%===================================================================
%% @doc
%% 启动子进程
%% @end
%%%===================================================================
startChild() ->
    {ok, Maps} = application:get_env('channel'),
    Num = maps:get(?DB_CHAN_NUM, Maps, 200),
    put(?DB_CHAN_NUM, Num),
    [begin
        F = fun(I, _) ->
            ChanName = makeChanName1(I),
            {ok, _} = supervisor:start_child(?MODULE, [ChanName]),
            ok
        end,
        c_lib:for(F, [], 1, Num + 1)
    end || supervisor:which_children(?MODULE) =:= []],
    ok.

%%%===================================================================
%% @doc
%% 构造chan 进程名
%% @end
%%%===================================================================
makeChanName1(Index) ->
    binary_to_atom(<<"db/chan/", (integer_to_binary(Index))/binary>>).
makeChanName2(Index) ->
    binary_to_existing_atom(<<"db/chan/", (integer_to_binary(Index))/binary>>).

%%%===================================================================
%% @doc
%% 获取pid
%% @end
%%%===================================================================
getPid(Index) ->
    Num = get(?DB_CHAN_NUM),
    ChanIndex = c_lib:get_loop_index(Index, Num),
    makeChanName2(ChanIndex).
