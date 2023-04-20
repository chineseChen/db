%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 27. 11月 2021 16:07
%%%-------------------------------------------------------------------
-module(uid_lib).
-author("chenkecai").

%% API
-export([get_uid/1, make_by_index/1]).
-export([make_uid/2, make_uid/3]).

%%%===================================================================
%% @doc
%%  获取index
%% @end
%%%===================================================================
get_uid(Key) ->
    F = fun(_, IdIndex) -> make_by_index(IdIndex) end,
    c_db_lib:update(local, uid, Key, F, dbT_uid:init(Key)).

%%%===================================================================
%% @doc
%% 获取index
%% @end
%%%===================================================================
make_by_index(Uid) ->
    Index = dbT_uid:get_index(Uid),
    NIndex = Index + 1,
    NIdIndex = dbT_uid:set_index(Uid, NIndex),
    {ok, NIndex, NIdIndex}.

%%%===================================================================
%% @doc
%%  index 增加head
%% @end
%%%===================================================================
make_uid(HeadId, IdIndex) ->
    <<Uid:64>> = <<HeadId:16, IdIndex:48>>, Uid.

%%%===================================================================
%% @doc
%% index 增加平台Id及渠道
%% @end
%%%===================================================================
make_uid(PId, Channel, IdIndex) ->
    <<Uid:64>> = <<PId:16, Channel:16, IdIndex:32>>, Uid.
