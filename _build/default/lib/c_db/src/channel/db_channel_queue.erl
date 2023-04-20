%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% 超出最大channel数后,channel队列
%%% @end
%%% Created : 24. 8月 2021 17:16
%%%-------------------------------------------------------------------
-module(db_channel_queue).
-author("chenkecai").
-include("db_channel.hrl").

%% API
-export([new_table/0, add/1, pick_up/0]).

%%%===================================================================
%% @doc
%%  创建表
%% @end
%%%===================================================================
new_table() ->
    ets:new(?MODULE, [ordered_set, protected, named_table, {keypos, 1}]).

%%%===================================================================
%% @doc
%%  增加渠道队列
%% @end
%%%===================================================================
add(Channel) ->
    ets:insert(?MODULE, {{Channel#channel.s_time, erlang:make_ref()}, Channel}).

%%%===================================================================
%% @doc
%% 拾取了一个排队channel
%% @end
%%%===================================================================
pick_up() ->
    NowMSec = c_time:now_millisecond(),
    pick_up_(NowMSec).
pick_up_(NowMSec) ->
    case ets:first(?MODULE) of
        '$end_of_table' -> ok;
        Key ->
            [{_, Channel}] = ets:lookup(?MODULE, Key),
            case NowMSec < Channel#channel.e_time of
                true -> [ets:delete(?MODULE, Key) || db_channel_m:create_channel(Channel, false)];
                false -> ets:delete(?MODULE, Key), pick_up_(NowMSec)
            end
    end.