%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 26. 11月 2021 18:16
%%%-------------------------------------------------------------------
-module(db_file_bat).
-author("chenkecai").

%% API
-export([do/0, do/1]).

do() ->
    do(none).

do([help]) ->
    io:format("clean all~n"),
    erlang:halt(), ok;
do(Arg) ->
    try
        NowMSec = c_time:now_millisecond(),
        [os:cmd("chcp 65001") || lists:member({encoding, latin1}, io:getopts()) andalso os:type() =:= {win32, nt}],
        io:format("start~n"),
        compile_load(),
        case Arg of
            [clean] -> db_2_file:remove_file();
            [all] ->
                db_2_file:remove_file(),
                un_load_code(),
                compile_load(),
                db_2_file:do();
            _ -> db_2_file:do()
        end,
        io:format("use millisecond ~p~n", [c_time:now_millisecond() - NowMSec]),
        io:format("end~n")
    catch
        EType:Reason:T ->
            io:format("do error, mod:~p ~p ~p ~p~n", [?MODULE, EType, Reason, T])
    end,
    erlang:halt(),
    ok.

%%%===================================================================
%% @doc
%%  编译加载需要文件
%% @end
%%%===================================================================
compile_load() ->
    code_lib:compile_load(
        [
            "apps/c_db/autoSrc/dbT/dbT.erl",
            "apps/c_bi/autoSrc/biT/biT.erl"
        ]),
    ok.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
un_load_code() ->
    code:purge('dbT'),
    code:delete('dbT').
