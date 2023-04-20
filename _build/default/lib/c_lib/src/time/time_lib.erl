%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 04. 8月 2021 15:25
%%%-------------------------------------------------------------------
-module(time_lib).
-author("chenkecai").

%% API
-export([now_second/0, now_millisecond/0, get_zero_time/1]).
-export([second_to_localtime/1, localtime_to_second/1]).
-export([set_offset_time/1, get_offset_time/0]).
-export([datetime_to_string/1]).

%%%===================================================================
%%% 设置时间偏移量
%%%===================================================================
set_offset_time(Value) ->
    KV = dbT_keyValue:init(time_offset),
    NKV = dbT_keyValue:set_value(KV, Value),
    c_db_lib:replace(local, keyValue, NKV).

%%%===================================================================
%%% 获取时间偏移量
%%%===================================================================
get_offset_time() ->
    KV = c_db_lib:get(local, keyValue, 'time_offset', dbT_keyValue:init(time_offset)),
    dbT_keyValue:get_value(KV).

%%%===================================================================
%%% 获取当前系统秒
%%%===================================================================
now_second() ->
    c_time:now_second() + get_offset_time().

%%%===================================================================
%%% 获当前系统豪秒
%%%===================================================================
now_millisecond() ->
    c_time:now_millisecond() + get_offset_time() * 1000.

%%%===================================================================
%%% 获取当天时间0点进间戳
%%%===================================================================
get_zero_time(Now) ->
    {_, {H, M, S}} = second_to_localtime(Now),%%获得日期
    Now - (H * 60 * 60 + M * 60 + S).

%%%===================================================================
%%% 转化为字符串
%%%===================================================================
datetime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
        [Year, Month, Day, Hour, Minute, Second])).

%%%===================================================================
%%% 转化为本地时间
%%%===================================================================
second_to_localtime(Sec) ->
    erlang:universaltime_to_localtime({calendar:gregorian_days_to_date(Sec div 86400 + 719528),
        calendar:seconds_to_time(Sec rem 86400)}).

%%%===================================================================
%%% 转化为时间戳
%%%===================================================================
localtime_to_second(DateTime) ->
    {Date, Time} = erlang:localtime_to_universaltime(DateTime),
    86400 * (calendar:date_to_gregorian_days(Date) - 719528) + calendar:time_to_seconds(Time).

