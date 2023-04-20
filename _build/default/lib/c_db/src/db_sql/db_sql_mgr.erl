%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_sql_mgr).

-behaviour(gen_server).
-include("db_channel.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(DB_SQL_LOOP_DOING_TIME_PICK, 60000).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    db_sql:init(),
    erlang:start_timer(?DB_SQL_LOOP_DOING_TIME_PICK, self(), 'timer_loop_do'),
    {ok, {}}.

handle_call('get_read_index', _From, State) ->
    RIndex = db_sql:getReadIndex(),
    {reply, RIndex, State};
handle_call({'get_db_name', DBSrc}, _From, State) ->
    RIndex = db_sql:getDbName(DBSrc),
    {reply, RIndex, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, _TimerRef, 'timer_loop_do'}, State) ->
    db_sql:ping(),
    erlang:start_timer(?DB_SQL_LOOP_DOING_TIME_PICK, self(), 'timer_loop_do'),
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
