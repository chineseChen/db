%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(dbChanProc).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-include("db_channel.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init([]) ->
    {ok, {}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(#channel{} = Request, State) ->
    dbChan:do(Request),
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
