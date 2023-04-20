%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2022, joinGame
%%% @doc
%%%
%%% @end
%%% Created : 16. 8月 2022 10:50
%%%-------------------------------------------------------------------
-module(c_db_util).
-author("chenkecai").

%% API
-export([format_sql_type/1, format_column_default/3, do_before_stop/0]).

%%%===================================================================
%% @doc
%% 构造sql type
%% @end
%%%===================================================================
format_sql_type(SqlType) ->
    c_lib:get_bool_value(is_tuple(SqlType), fun() ->
        {Type, Num} = SqlType, io_lib:format("~s(~w)", [Type, Num])
    end, SqlType).

%%%===================================================================
%% @doc
%% 构造字段默认字段
%% @end
%%%===================================================================
format_column_default(_EType, 'BLOB', _Default) -> "";
format_column_default(_EType, 'TEXT', _Default) -> "";
format_column_default(EType, _SqlType, Default) ->
    DefaultVar = c_lib:get_bool_value(EType =:= 'integer',
        fun() -> integer_to_list(Default) end,
        fun() ->
            case EType =:= 'string' orelse EType =:= 'binary' of
                true -> Default;
                false -> str_lib:term_2_str(Default)
            end
        end),
    io_lib:format(" DEFAULT '~ts'", [DefaultVar]).

%%%===================================================================
%% @doc
%% app停止前先保证所有数据持久化
%% @end
%%%===================================================================
do_before_stop() ->
    %% 停上服务器操作
    supervisor:terminate_child('c_db_sup', 'db_handle_proc'),
    %% 先关闭处理进程
    supervisor:terminate_child('c_db_sup', 'db_channel_sup'),
    %% 持久化操作
    db_cache_m:doBeforeTerminate(),
    ok.

