%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_lock_proc).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-include("db_channel.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    db_lock:new_table(),
    {ok, {}}.

handle_call({'queue_doing', Args}, _From, State) ->
    Reply = db_lock:doing(Args),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({'delete_channel', Args}, State) ->
    db_lock:del_channel(Args),
    {noreply, State};
handle_cast({'clear_lock', Args}, State) ->
    db_lock:clearLock(Args),
    {noreply, State};
handle_cast({'delClearLock', Args}, State) ->
    db_lock:delClearChannel(Args),
    {noreply, State};
handle_cast({'do_lock', Args}, State) ->
    db_lock:doing(Args),
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
