%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 24. 8月 2021 10:56
%%%-------------------------------------------------------------------
-module(db_channel_m).
-author("chenkecai").

%% API
-export([init_channel/1, next_msg_do/2, create_channel/2]).
-export([initChanIndex/0]).
-include("db_channel.hrl").
-define(DB_CHAN_INDEX, '$chan_index').
-define(DB_MAX_CHANNEL_NUM, 'channel_max_num').
-define(CHANNEL_PROC_HANDLE_MIN_TIME_OUT, 1000).
-define(CHANNEL_PROC_HANDLE_MAX_TIME_OUT, 5 * 60 * 1000).

%%%===================================================================
%% @doc
%%  初始渠道信息
%% @end
%%%===================================================================
init_channel(Msg) ->
    NowMSec = c_time:now_millisecond(),
    #{type := Type, tab_keys := TabKeys, from := From} = Msg,
    %%操作超时区间1秒~5分钟
    RealTimeOut = min(maps:get('time_out', Msg, ?CHANNEL_PROC_HANDLE_MIN_TIME_OUT), ?CHANNEL_PROC_HANDLE_MAX_TIME_OUT),
    Info = maps:get('info', Msg, []),
    Channel = #channel{s_time = NowMSec, time_out = RealTimeOut,
        e_time = NowMSec + RealTimeOut, type = Type, tab_keys = TabKeys, from = From, info = Info},
%%    create_channel(Channel, true).
    case maps:get('chanFlag', Msg, false) of
        true ->
            Index = getChanIndex(),
            Pid = dbChanSup:getPid(Index),
            gen_server:cast(Pid, Channel),
            ok;
        false -> create_channel(Channel, true)
    end.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
next_msg_do(Pid, #{from := From} = Msg) ->
    case is_process_alive(Pid) of
        true -> gen_server:cast(Pid, Msg);
        false -> gen_server:reply(From, #{state => 'stop', error => 'channel_error', reason => "maybe handle time out"})
    end.

%%%===================================================================
%% @doc
%%  创建channel
%% @end
%%%===================================================================
create_channel(Channel, AddBool) ->
    CountL = supervisor:count_children('db_channel_sup'),
    Count = proplists:get_value('workers', CountL, 0),
    {ok, Maps} = application:get_env('channel'),
    MaxChannelNum = maps:get(?DB_MAX_CHANNEL_NUM, Maps, 100000),
    case Count < MaxChannelNum of
        true ->
            {ok, Pid} = db_channel_sup:start_child(),
            gen_server:cast(Pid, Channel), true;
        false ->
            [db_channel_queue:add(Channel) || AddBool], false
    end.

%%%===================================================================
%% @doc
%% 初始化chan index
%% @end
%%%===================================================================
initChanIndex() ->
    put(?DB_CHAN_INDEX, 0).

%%%===================================================================
%% @doc
%% 获取chan的增长系数
%% @end
%%%===================================================================
getChanIndex() ->
    Index = get(?DB_CHAN_INDEX) + 1,
    put(?DB_CHAN_INDEX, Index),
    Index.

