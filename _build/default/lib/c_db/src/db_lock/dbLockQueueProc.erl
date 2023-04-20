%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(dbLockQueueProc).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, {}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({free_lock_queue, FreeL}, State) ->
    dbLockQueue:freeQueue(FreeL),
    {noreply, State};
handle_cast({free_clear, STag}, State) ->
    dbLockQueue:freeAllQueue(STag),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
