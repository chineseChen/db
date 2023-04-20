%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_cache_mgr).

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
    db_cache_m:new_table(),
    db_cache_m:init_tab(),
    db_cache_m:initIndex(),
    {ok, {}}.

handle_call('make_change_tab_id', _From, State) ->
    Reply = db_cache_m:getIdByIndex(),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast('re_calc_cache_change', State) ->
    db_cache_m:reCalcCacheChange(),
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
