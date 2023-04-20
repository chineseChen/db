%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2022, joinGame
%%% @doc
%%%
%%% @end
%%% Created : 28. 9月 2022 10:57
%%%-------------------------------------------------------------------
-module(uid_maker).
-author("chenkecai").
-define(UID_TYPE_USER, 'uid_type_account').

%% API
-export([makeUserId/2]).

%%%===================================================================
%% @doc
%% 生成巨量平台uid
%% @end
%%%===================================================================
makeUserId(PId, Channel) ->
    Uid = uid_lib:get_uid(?UID_TYPE_USER),
    uid_lib:make_uid(PId, Channel, Uid).
