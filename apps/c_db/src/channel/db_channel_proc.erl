%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_channel_proc).

-behaviour(gen_server).
-include("db_channel.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([re_set_timer/2]).

-define(CHANNEL_PROC_TIME_OUT, 60 * 1000). %%1分进程没有收到任何消息进程退出
%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, {}, ?CHANNEL_PROC_TIME_OUT}.

handle_call('unlock', From, State) ->
    db_channel:unlock(From),
    {noreply, State, ?CHANNEL_PROC_TIME_OUT};
handle_call(_Request, _From, State) ->
    {noreply, State, ?CHANNEL_PROC_TIME_OUT}.

handle_cast({shutdown, Reason}, State) ->
    {stop, {shutdown, Reason}, State};
handle_cast('clear_ok', State) ->
    {stop, {shutdown, normal}, State};
handle_cast(Request, State) ->
    cast_dispatch(Request),
    {noreply, State, ?CHANNEL_PROC_TIME_OUT}.

handle_info({timeout, _TimerRef, 'handle_time_out'}, State) ->
    db_channel:handle_time_out(),
    {'stop', {'shutdown', 'handle_time_out'}, State};
handle_info(timeout, State) ->
    logger:error("mod:~p line:~p~nreason:~p", [?MODULE, ?LINE, erlang:process_info(self())]),
    {'stop', {shutdown, 'not_msg_receive'}, State};
handle_info(_Info, State) ->
    {noreply, State, ?CHANNEL_PROC_TIME_OUT}.

terminate(Reason, _State) ->
    gen_server:cast('db_channel_mgr', 'channel_pid_exit'),
    case Reason of
        'normal' -> ok;
        'shutdown' -> ok;
        {'shutdown', 'normal'} -> ok;
        {'shutdown', _} ->
            db_channel:delChannel(Reason);
        _ ->
            logger:error("mod:~p line:~p~nreason:~p~npid:~p", [?MODULE, ?LINE, Reason, erlang:process_info(self())]),
            db_channel:delChannel(Reason)
    end, ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
cast_dispatch(#channel{} = Channel) ->
    TimeOut = max(Channel#channel.time_out, 0),
    TimerRef = erlang:start_timer(TimeOut, self(), 'handle_time_out'),
    db_channel:do(Channel#channel{timer_ref = TimerRef});
cast_dispatch(#{type := 'iterate_next', from := From}) ->
    db_channel:iterate_next(From);
cast_dispatch(#{type := 'iterate_end', from := From}) ->
    db_channel:iterate_end(From);
cast_dispatch(#{} = Msg) ->
    db_channel:do_end(Msg);
cast_dispatch('lock_ok') ->
    db_channel:lock_ok();
cast_dispatch('unlock') ->
    db_channel:unlock();
cast_dispatch('tryClearTab') ->
    db_channel:tryClearTab();
cast_dispatch({'clearPidQueue', Pid, ETime}) ->
    db_channel:clearQueue(Pid, ETime);
cast_dispatch('clearQueueReply') ->
    db_channel:clearQueueReply();
cast_dispatch(_) -> ok.

%%%===================================================================
%% @doc
%% 重置handle操时
%% @end
%%%===================================================================
re_set_timer(TimerRef, TimeOut) ->
    case erlang:cancel_timer(TimerRef) of
        false -> false;
        _ -> erlang:start_timer(TimeOut, self(), 'handle_time_out')
    end.