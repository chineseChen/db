%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_channel_mgr).

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
    db_channel_queue:new_table(),
    dbChanSup:startChild(),
    db_channel_m:initChanIndex(),
    {ok, {}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(#{pid := Pid} = Msg, State) ->
    db_channel_m:next_msg_do(Pid, Msg),
    {noreply, State};
handle_cast(#{} = Msg, State) ->
    db_channel_m:init_channel(Msg),
    {noreply, State};
handle_cast('channel_pid_exit', State) ->
    db_channel_queue:pick_up(),
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
