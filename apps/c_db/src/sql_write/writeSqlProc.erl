%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2022, <joinGames>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(writeSqlProc).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(DB_WRITE_SQL_PROC_LOOP_INTERVAL, 1000).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(ConCfg) ->
    gen_server:start_link(?MODULE, [ConCfg], []).

init([ConCfg]) ->
    writeSql:init(ConCfg),
    erlang:start_timer(?DB_WRITE_SQL_PROC_LOOP_INTERVAL, self(), 'dbWriteSqlProcLoop'),
    {ok, {}}.

handle_call({'query_by_sql', Msg}, _From, State) ->
    writeSql:queryBySql(Msg),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast('try_write_start', State) ->
    writeSql:tryStart(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(ping, State) ->
    writeSql:ping(),
    {noreply, State};
handle_info({timeout, _TimerRef, 'dbWriteSqlProcLoop'}, State) ->
    writeSql:cleanProcState(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    writeSql:deleteProcState(),
    writeSql:cleanEnv(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
