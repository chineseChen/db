%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2022, <joinGames>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(writeSqlMgr).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(DB_TAKE_CLEAN_SQL_DATA_INTERVAL, 1000).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    takeSql:init(),
    erlang:start_timer(?DB_TAKE_CLEAN_SQL_DATA_INTERVAL, self(), 'db_clean_take_sql_data'),
    {ok, {}}.

handle_call({'take_1', ChangeId, Pid}, _From, State) ->
    Reply = takeSql:take_1(ChangeId, Pid),
    {reply, Reply, State};
handle_call({'take_2', Msg}, _From, State) ->
    Reply = takeSql:takeKey_2(Msg),
    {reply, Reply, State};
handle_call({'takeMulKey_2', Msg}, _From, State) ->
    Reply = takeSql:takeMulKey_2(Msg),
    {reply, Reply, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast('writeBySql', State) ->
    takeSql:write(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, _TimerRef, 'db_clean_take_sql_data'}, State) ->
    takeSql:timeOutClean(),
    erlang:start_timer(?DB_TAKE_CLEAN_SQL_DATA_INTERVAL, self(), 'db_clean_take_sql_data'),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
