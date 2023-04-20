%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2022, <joinGames>
%%% @doc
%%%
%%% @end
%%% Created : 20. 12月 2022 17:22
%%%-------------------------------------------------------------------
-module(hotSys).
-author("ckc").

%% API
-export([do/0]).

%%%===================================================================
%% @doc
%% 系统热更
%% @end
%%%===================================================================
do() ->
    hot_code:reload(),
    case whereis(dataMgr) of
        Pid when Pid =/= undefined ->
            gen_server:call(Pid, hotCode);
        _ -> ok
    end,
    case whereis(cTimerMgr) of
        Pid1 when Pid1 =/= undefined ->
            gen_server:call(Pid1, hotCode);
        _ -> ok
    end,
    ok.
