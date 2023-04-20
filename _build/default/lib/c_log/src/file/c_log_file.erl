%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 04. 8月 2021 16:28
%%%-------------------------------------------------------------------
-module(c_log_file).
-author("chenkecai").
-define(LOG_PATH, "./log/").

%% API
-export([read_files_name/1, try_create_file/3, time_doing/2, open_file/1]).
%%-export([create_file/2]).

%%%===================================================================
%%% 读取所有的包含tag的log文件
%%%===================================================================
read_files_name(Tag) ->
    TimeMbStr = "\\d{4,}-(?:1[0-2]|0[1-9])-(?:0[1-9]|[1-2][0-9]|3[0-1])_(?:0[0-9]|1[0-9]|2[0-3])-(?:0[0-9]|[1-5][0-9])",
    FileMbStr = "^" ++ atom_to_list(Tag) ++ "_" ++ TimeMbStr ++ ".log$",
    {ok, TimeMB} = re:compile(TimeMbStr, [caseless]),
    {ok, FileMB} = re:compile(FileMbStr, [caseless]),
    {ok, FileNameL} = file:list_dir(?LOG_PATH),
    F = fun(FileName, Acc) ->
        case re:run(FileName, FileMB, [global, {capture, all, list}]) of
            {match, [[Match1st]]} ->
                {match, [[TimeStr]]} = re:run(FileName, TimeMB, [global, {capture, all, list}]),
                CreateTime = str_2_time(TimeStr),
                LogInfo = #{key => Match1st, create_time => CreateTime, tag => Tag},
                o_tuple:set(Match1st, LogInfo, Acc);
            _ -> Acc
        end
    end,
    lists:foldl(F, {}, FileNameL).

%%%===================================================================
%%% 创建当前时段文件
%%%===================================================================
try_create_file(Tag, {}, _LoopTime) ->
    NowSec = c_time:now_second(),
    FileName = create_file(Tag, NowSec),
    LogInfo = #{key => FileName, create_time => NowSec, tag => Tag},
    NLogInfo = o_tuple:set(FileName, LogInfo, {}),
    File = ?LOG_PATH ++ FileName,
    {ok, File, NLogInfo};
try_create_file(Tag, FileInfo, LoopTime) ->
    #{create_time := CreateTime, key := LastFileName} = element(size(FileInfo), FileInfo),
    ZeroTime = time_lib:get_zero_time(CreateTime),
    NowSec = c_time:now_second(),
    DiffTime = CreateTime - ZeroTime,
    NextTime = ZeroTime + (DiffTime div LoopTime + 1) * LoopTime,
    case NowSec >= NextTime of
        true ->
            FileName = create_file(Tag, NowSec),
            LogInfo = #{key => FileName, create_time => NowSec, tag => Tag},
            NLogInfo = o_tuple:set(FileName, LogInfo, FileInfo),
            File = ?LOG_PATH ++ FileName,
            {ok, File, NLogInfo};
        false ->
            File = ?LOG_PATH ++ LastFileName,
            {ok, File}
    end.

%%%===================================================================
%%% 定时循环执行
%%%===================================================================
time_doing(FileInfo, Cfg) ->
    #{id := Tag, save_file_time := SaveFileTime, file_loop_time := LoopTime, fd := OldFd} = Cfg,
    NowSec = c_time:now_second(),
    TimeOut = NowSec - SaveFileTime,
    {DelBool, NFileInfo0} = case del_time_out_file(FileInfo, TimeOut) of
        {ok, FileInfoVar} -> {true, FileInfoVar};
        'no_change' -> {false, FileInfo}
    end,
    case try_create_file(Tag, NFileInfo0, LoopTime) of
        {ok, File, NFileInfo} ->
            {ok, Fd} = open_file(File),
            file:close(OldFd),
            {ok, NFileInfo, Cfg#{file := File, fd => Fd}};
        {ok, _File} ->
            case DelBool of
                true -> {ok, NFileInfo0, Cfg};
                false -> {ok, Cfg}
            end
    end.

%%%===================================================================
%%% 获取文件
%%%===================================================================
open_file(File) ->
    file:open(File, [append, {encoding, utf8}]).

%%%===================================================================
%%% 解析时间
%%%===================================================================
str_2_time(TimeStr) ->
    str_2_time(TimeStr, [], []).

str_2_time([], NumL, R) ->
    [V1, V2, V3, V4, V5] = lists:reverse([list_to_integer(lists:reverse(NumL)) | R]),
    time_lib:localtime_to_second({{V1, V2, V3}, {V4, V5, 0}});
str_2_time([H | T], NumL, R) when H >= $0 andalso H =< $9 ->
    str_2_time(T, [H | NumL], R);
str_2_time([_ | T], [_ | _] = NumL, R) ->
    str_2_time(T, [], [list_to_integer(lists:reverse(NumL)) | R]);
str_2_time([_ | T], [], R) ->
    str_2_time(T, [], R).

%%%===================================================================
%%% 创建
%%%===================================================================
create_file(Tag, Time) ->
    {{Year, Month, Day}, {Hour, Minute, _}} = time_lib:second_to_localtime(Time),
    atom_to_list(Tag) ++ "_" ++ lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w_~2..0w-~2..0w",
        [Year, Month, Day, Hour, Minute])) ++ ".log".

%%%===================================================================
%%% 删除过期文件
%%%===================================================================
del_time_out_file(FileInfo, TimeOut) ->
    case size(FileInfo) > 1 of
        true ->
            #{create_time := CreateTime, key := Filename} = element(1, FileInfo),
            case CreateTime =< TimeOut of
                true ->
                    file:delete(?LOG_PATH ++ Filename),
                    {ok, erlang:delete_element(1, FileInfo)};
                false ->
                    'no_change'
            end;
        false ->
            'no_change'
    end.