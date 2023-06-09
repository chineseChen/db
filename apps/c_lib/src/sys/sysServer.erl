%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%%
%%% @end
%%% Created : 01. 2月 2023 9:59
%%%-------------------------------------------------------------------
-module(sysServer).
-author("ckc").

%% API
-export([getApps/1]).

%%%===================================================================
%% @doc
%% 获取服务器对映app
%% @end
%%%===================================================================
getApps(_) ->
    {[c_log, c_lib, c_db], [game]}.