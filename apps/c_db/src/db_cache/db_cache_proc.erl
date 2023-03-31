%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_cache_proc).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-include("db_channel.hrl").
-define(TIME_LOOP_CLEAR_INTERVAL, 1000).
-define(TIME_LOOP_DOING_DATA_COUNT, 1000).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(ModT) ->
    gen_server:start_link(?MODULE, ModT, []).

init({STag, Mod}) ->
    Arg0 = #cacheMgr{pid = self()},
    Arg1 = db_cache:setSupTagMod(STag, Mod, Arg0),
    Arg2 = db_cache:new_tab(Mod, Arg1),
    db_cache_m:tab_insert(Arg2),
    db_cache:init_key(),
    erlang:start_timer(?TIME_LOOP_CLEAR_INTERVAL, self(), 'cache_time_loop_clear'),
    {ok, {}}.

handle_call('format_cache_arg', _From, State) ->
    Reply = db_cache:formatArg(),
    {reply, Reply, State};
handle_call('max_change_index', _From, State) ->
    Reply = db_cache:maxChangeIndex(),
    {reply, Reply, State};
handle_call({'insert_data', Msg}, _From, State) ->
    Reply = db_cache:insertData(Msg),
    {reply, Reply, State};
handle_call({'write_start', Msg}, _From, State) ->
    Reply = db_cache:writeStart(Msg),
    {reply, Reply, State};
handle_call({'write_end', Msg}, _From, State) ->
    Reply = db_cache:writeEnd(Msg),
    {reply, Reply, State};
handle_call({'clear_data', Msg}, _From, State) ->
    Reply = db_cache:clearData(Msg),
    {reply, Reply, State};
handle_call('do_before_terminate', _From, State) ->
    db_cache:terminate(),
    put('terminate_did_flag', true),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({'insert', Msg}, State) ->
    db_cache:insert(Msg),
    {noreply, State};
handle_cast({'update_cache_info', Msg}, State) ->
    db_cache:update_cache_info(Msg),
    {noreply, State};
handle_cast({'roll_back', Msg}, State) ->
    db_cache:rollBack(Msg),
    {noreply, State};
handle_cast('re_calc_1_cache_change', State) ->
    db_cache:reCalcCacheChange(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, _TimerRef, 'cache_time_loop_save'}, State) ->
    db_cache:timerLoopSave(),
    {noreply, State};
handle_info({timeout, _TimerRef, 'cache_time_loop_clear'}, State) ->
    db_cache:timerLoopClear(?TIME_LOOP_DOING_DATA_COUNT),
    erlang:start_timer(?TIME_LOOP_CLEAR_INTERVAL, self(), 'cache_time_loop_clear'),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    [db_cache:terminate() || get('terminate_did_flag') =:= undefined],
    db_cache_m:tab_del(db_cache:getCacheTag()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
