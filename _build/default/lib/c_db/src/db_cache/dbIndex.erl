%%%-------------------------------------------------------------------
%%% @author ckc
%%% @copyright (C) 2023, <joinGames>
%%% @doc
%%%
%%% @end
%%% Created : 07. 4月 2023 13:54
%%%-------------------------------------------------------------------
-module(dbIndex).
-author("ckc").
-include("db_channel.hrl").

%% API
-export([createTab/1]).
-export([getPos1/1, getPos2/1, makeTabName/2]).

%%%===================================================================
%% @doc
%% 创建表
%% @end
%%%===================================================================
createTab(Tag) ->
    case getPos1(Tag) of
        [_ | _] = L ->
            F = fun(Index) ->
                TabName = makeTabName(Tag, Index),
                ets:new(TabName, [ordered_set, protected, {keypos, 1}])
            end,
            [F(Index) || Index <- L];
        [] -> []
    end.

%%%===================================================================
%% @doc
%% 检查tag是否存在索引
%% @end
%%%===================================================================
getPos1(Tag) ->
    T = dbT:get_mod(),
    case o_tuple:get(Tag, T) of
        {_, Mod} when is_atom(Mod) ->
            getPos2(Mod);
        _ -> []
    end.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
getPos2(Mod) ->
    case erlang:function_exported(Mod, indexPos, 0) of
        true ->
            Pos = Mod:indexPos(),
            ?BOOL_VALUE(is_list(Pos), Pos, [Pos]);
        false -> []
    end.

%%%===================================================================
%% @doc
%% 构造ets tab name
%% @end
%%%===================================================================
makeTabName(Tag, Index) ->
    TagBin = atom_to_binary(Tag),
    IndexBin = integer_to_binary(Index),
    binary_to_atom(<<TagBin/binary, "/index/tab/", IndexBin/binary>>).