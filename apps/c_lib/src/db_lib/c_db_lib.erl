%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 30. 9月 2021 16:20
%%%-------------------------------------------------------------------
-module(c_db_lib).
-author("chenkecai").
-define(HANDLE_DEFAULT_TIME_OUT, 5000).
-define(HANDLE_CLEAR_DEFAULT_TIME_OUT, 30000).

%% API
-export([member/3, member/4]).
-export([get_all_k/2, get_all_k/3, get_values/3, get_values/4]). %% 非调试禁用
-export([get_count/2, get_count/3]).
%%
-export([key1st/2, key1st/3, key1st/4]).
-export([index1st/3, index1st/4, index1st/5]).
-export([lastKey/2, lastKey/3, lastKey/4]).
-export([lastIndex/3, lastIndex/4, lastIndex/5]).
%%
-export([getKey/4, getKey/5, getKey/6, getKeyL/3, getKeyL/4, getKeyL/5]).
-export([get/3, get/4, get/5]).
-export([m_get/3, m_get/4, m_get/5]).
-export([replace/3, replace/4, update/4, update/5, update/6, update/7]).
-export([replace_/3, replace_/4]).
-export([handle/2, handle/3, handle/4]).
-export([delete/3, delete/4]).
-export([delete_/3, delete_/4]).
-export([iterateIndex/5, iterateIndex/6]).
-export([iterate/4, iterate/5]).
-export([clear/2, clear/3]).

%%%===================================================================
%% @doc
%%  获取所有k, 非调试禁用
%% @end
%%%===================================================================
get_all_k(Src, Table) ->
    get_all_k(Src, Table, ?HANDLE_DEFAULT_TIME_OUT).
get_all_k(Src, Table, TimeOut) ->
    db_handle_c:get_all_k(Src, Table, TimeOut).

%%%===================================================================
%% @doc
%%  获取所有数据,非调试禁用
%% @end
%%%===================================================================
get_values(Src, Table, Num) ->
    get_values(Src, Table, Num, ?HANDLE_DEFAULT_TIME_OUT).
get_values(Src, Table, Num, TimeOut) when is_integer(Num) andalso Num > 0 ->
    db_handle_c:get_values(Src, Table, Num, TimeOut).

%%%===================================================================
%% @doc
%% key是否存在,比get速度快
%% @end
%%%===================================================================
member(Src, Table, Key) ->
    member(Src, Table, Key, ?HANDLE_DEFAULT_TIME_OUT).
member(Src, Table, Key, TimeOut) ->
    db_handle_c:member(Src, Table, Key, TimeOut).

%%%===================================================================
%% @doc
%%  获取数据数量
%% @end
%%%===================================================================
get_count(Src, Table) ->
    get_count(Src, Table, ?HANDLE_DEFAULT_TIME_OUT).
get_count(Src, Table, TimeOut) ->
    db_handle_c:get_count(Src, Table, TimeOut).

%%%===================================================================
%% @doc
%%  获取数据
%% @end
%%%===================================================================
get(Src, Table, Key) ->
    get(Src, Table, Key, none).
get(Src, Table, Key, Default) ->
    get(Src, Table, Key, Default, ?HANDLE_DEFAULT_TIME_OUT).
get(Src, Table, Key, Default, TimeOut) ->
    db_handle_c:get(Src, Table, Key, Default, TimeOut).

%%%===================================================================
%% @doc
%% 最小key
%% @end
%%%===================================================================
key1st(Src, Table) ->
    key1st(Src, Table, none).
key1st(Src, Table, Default) ->
    key1st(Src, Table, Default, ?HANDLE_DEFAULT_TIME_OUT).
key1st(Src, Table, Default, TimeOut) ->
    db_handle_c:key1st(Src, Table, Default, TimeOut).

%%%===================================================================
%% @doc
%% 最小Index
%% @end
%%%===================================================================
index1st(Src, Table, Pos) ->
    index1st(Src, Table, Pos, none).
index1st(Src, Table, Pos, Default) ->
    index1st(Src, Table, Pos, Default, ?HANDLE_DEFAULT_TIME_OUT).
index1st(Src, Table, Pos, Default, TimeOut) ->
    db_handle_c:index1st(Src, Table, Pos, Default, TimeOut).

%%%===================================================================
%% @doc
%% 最大key
%% @end
%%%===================================================================
lastKey(Src, Table) ->
    lastKey(Src, Table, none).
lastKey(Src, Table, Default) ->
    lastKey(Src, Table, Default, ?HANDLE_DEFAULT_TIME_OUT).
lastKey(Src, Table, Default, TimeOut) ->
    db_handle_c:lastKey(Src, Table, Default, TimeOut).

%%%===================================================================
%% @doc
%% 最小Index
%% @end
%%%===================================================================
lastIndex(Src, Table, Pos) ->
    lastIndex(Src, Table, Pos, none).
lastIndex(Src, Table, Pos, Default) ->
    lastIndex(Src, Table, Pos, Default, ?HANDLE_DEFAULT_TIME_OUT).
lastIndex(Src, Table, Pos, Default, TimeOut) ->
    db_handle_c:lastIndex(Src, Table, Pos, Default, TimeOut).

%%%===================================================================
%% @doc
%% 获取key
%% @end
%%%===================================================================
getKey(Src, Table, Pos, Index) ->
    [Var] = getKeyL(Src, Table, [{Pos, Index}]), Var.
getKey(Src, Table, Pos, Index, Default) ->
    [Var] = getKeyL(Src, Table, [{Pos, Index}], Default), Var.
getKey(Src, Table, Pos, Index, Default, TimeOut) ->
    [Var] = getKeyL(Src, Table, [{Pos, Index}], Default, TimeOut), Var.
getKeyL(Src, Table, PosIndexL) ->
    getKeyL(Src, Table, PosIndexL, none).
getKeyL(Src, Table, PosIndexL, Default) ->
    getKeyL(Src, Table, PosIndexL, Default, ?HANDLE_DEFAULT_TIME_OUT).
getKeyL(Src, Table, PosIndexL, Default, TimeOut) ->
    db_handle_c:getKey(Src, Table, PosIndexL, Default, TimeOut).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
m_get(Src, Table, Keys) ->
    m_get(Src, Table, Keys, none).
m_get(Src, Table, Keys, Default) ->
    m_get(Src, Table, Keys, Default, ?HANDLE_DEFAULT_TIME_OUT).
m_get(Src, Table, Keys, Default, TimeOut) ->
    db_handle_c:m_get(Src, Table, Keys, Default, TimeOut).

%%%===================================================================
%% @doc
%%  更新数据
%% @end
%%%===================================================================
replace(Src, Table, Value) ->
    replace(Src, Table, Value, ?HANDLE_DEFAULT_TIME_OUT).
replace(Src, Table, Value, TimeOut) ->
    ModT = dbT:get_mod(),
    Key = case o_tuple:get(Table, ModT) of
        {_, [_ | _]} -> 1;
        {_, Mod} ->
            KeyIndex = Mod:key_index(),
            KeyI = case is_tuple(KeyIndex) of
                true -> element(1, KeyIndex);
                false -> KeyIndex
            end,
            element(KeyI, Value)
    end,
    db_handle_c:replace(Src, Table, Key, Value, TimeOut).

%%%===================================================================
%% @doc
%%  更新数据,返回old数据
%% @end
%%%===================================================================
replace_(Src, Table, Value) ->
    replace_(Src, Table, Value, ?HANDLE_DEFAULT_TIME_OUT).
replace_(Src, Table, Value, TimeOut) ->
    ModT = dbT:get_mod(),
    Key = case o_tuple:get(Table, ModT) of
        {_, Mod} when is_atom(Mod) ->
            KeyIndex = Mod:key_index(),
            KeyI = case is_tuple(KeyIndex) of
                true -> element(1, KeyIndex);
                false -> KeyIndex
            end,
            element(KeyI, Value);
        _ -> 1
    end,
    db_handle_c:replace_(Src, Table, Key, Value, TimeOut).

%%%===================================================================
%% @doc
%% 删除数据
%% @end
%%%===================================================================
delete(Src, Table, Key) ->
    delete(Src, Table, Key, ?HANDLE_DEFAULT_TIME_OUT).
delete(Src, Table, Key, TimeOut) ->
    db_handle_c:delete(Src, Table, Key, TimeOut).

%%%===================================================================
%% @doc
%% 删除数据,返回old数据
%% @end
%%%===================================================================
delete_(Src, Table, Key) ->
    delete_(Src, Table, Key, ?HANDLE_DEFAULT_TIME_OUT).
delete_(Src, Table, Key, TimeOut) ->
    db_handle_c:delete_(Src, Table, Key, TimeOut).

update(Src, Table, Key, F) ->
    update(Src, Table, Key, F, none, [], ?HANDLE_DEFAULT_TIME_OUT).
update(Src, Table, Key, F, Default) ->
    update(Src, Table, Key, F, Default, [], ?HANDLE_DEFAULT_TIME_OUT).
update(Src, Table, Key, F, Default, A) ->
    update(Src, Table, Key, F, Default, A, ?HANDLE_DEFAULT_TIME_OUT).
update(Src, Table, Key, F, Default, A, TimeOut) ->
    {ok, Value, ResMap} = db_handle_c:update_1(Src, Table, Key, Default, TimeOut),
    try F(A, Value) of
        {ok, Res} ->
            db_handle_c:update_2(ResMap), Res;
        {ok, Res, NValue} ->
            db_handle_c:update_2(ResMap, NValue), Res
    catch
%%        Error ->
%%            db_handle_c:update_2(ResMap), Error;
        Class:Reason:Stacktrace ->
            db_handle_c:update_2(ResMap),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%% @doc
%%  事务操作数据据库
%% @end
%%%===================================================================
%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
-spec handle(TabKeys, Func) -> term() when
    TabKeys :: [{atom(), atom(), term(), term()}]| [{atom(), atom(), term()}],
    Func :: fun().
handle(TabKeys, F) when is_function(F) ->
    handle(TabKeys, F, [], ?HANDLE_DEFAULT_TIME_OUT).

-spec handle(TabKeys, Func, integer()) -> term() when
    TabKeys :: [{atom(), atom(), term(), term()}],
    Func :: fun().
handle(TabKeys, F, A) when is_function(F) ->
    handle(TabKeys, F, A, ?HANDLE_DEFAULT_TIME_OUT).
handle([_ | _] = TabKeys, F, A, TimeOut) ->
    {ok, DataL, ResMap} = db_handle_c:handle_1(TabKeys, TimeOut),
    try F(A, DataL) of
        {ok, Res} ->
            db_handle_c:handle_2(ResMap), Res;
        {ok, Res, NDataL} ->
            db_handle_c:handle_2(ResMap, NDataL), Res;
        {ok, Res, NDataL, IDataL} ->
            db_handle_c:handle_2(ResMap, NDataL, IDataL), Res
    catch
%%        Error ->
%%            db_handle_c:handle_2(ResMap), Error;
        Class:Reason:Stacktrace ->
            db_handle_c:handle_2(ResMap),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%% @doc
%%  遍历 IterType:ascending|descending
%% @end
%%%===================================================================
iterate(Src, Table, F, Arg) ->
    iterate(Src, Table, F, Arg, #{info => [{'sortType', 'ascending'}]}).
iterate(Src, Table, F, Arg, 'descending') ->
    iterate(Src, Table, F, Arg, #{info => [{'sortType', 'descending'}]});
iterate(Src, Table, F, Arg, Map) ->
    db_handle_c:is_tab(Src, Table),
    NMap0 = case Map of
        #{time_out := _} -> Map;
        _ -> Map#{time_out => ?HANDLE_DEFAULT_TIME_OUT}
    end,
    NMap = NMap0#{type => 'iterate', tab_keys => {Src, Table}},
    iterate_(Src, Table, F, Arg, NMap).
iterate_(Src, Table, F, Arg, Map) ->
    {ok, NMap, Key} = db_handle_c:iterate(Map),
    case Key of
        '$end_of_table' -> Arg;
        _ ->
            case F(Src, Key, Arg) of
                {break, NArg} ->
                    db_handle_c:iterate_end(NMap#{type => 'iterate_end'}),
                    NArg;
                {ok, NArg} -> iterate_(Src, Table, F, NArg, NMap#{type => 'iterate_next'});
                NArg -> iterate_(Src, Table, F, NArg, NMap#{type => 'iterate_next'})
            end
    end.

%%%===================================================================
%% @doc
%% 通过索引遍历
%% @end
%%%===================================================================
iterateIndex(Src, Table, Index, F, Arg) ->
    iterateIndex(Src, Table, Index, F, Arg, #{info => [{'sortType', 'ascending'}]}).
iterateIndex(Src, Table, Index, F, Arg, 'descending') ->
    iterateIndex(Src, Table, Index, F, Arg, #{info => [{'sortType', 'descending'}]});
iterateIndex(Src, Table, Index, F, Arg, Map) ->
    db_handle_c:checkIterateIndex(Src, Table, Index),
    NMap0 = case Map of
        #{time_out := _} -> Map;
        _ -> Map#{time_out => ?HANDLE_DEFAULT_TIME_OUT}
    end,
    InfoL = maps:get(info, NMap0, []),
    NMap1 = NMap0#{info => [{index, Index} | InfoL]},
    NMap = NMap1#{type => 'iterateIndex', tab_keys => {Src, Table}},
    iterateIndex_(Src, Table, F, Arg, NMap).
iterateIndex_(Src, Table, F, Arg, Map) ->
    {ok, NMap, Keys} = db_handle_c:iterate(Map),
    case Keys of
        '$end_of_table' -> Arg;
        _ ->
            case F(Src, Keys, Arg) of
                {break, NArg} ->
                    db_handle_c:iterate_end(NMap#{type => 'iterate_end'}),
                    NArg;
                {ok, NArg} -> iterateIndex_(Src, Table, F, NArg, NMap#{type => 'iterateIndexNext'});
                NArg -> iterateIndex_(Src, Table, F, NArg, NMap#{type => 'iterateIndexNext'})
            end
    end.

%%%===================================================================
%% @doc
%%  清理
%% @end
%%%===================================================================
clear(Src, Table) ->
    clear(Src, Table, ?HANDLE_CLEAR_DEFAULT_TIME_OUT).
clear(Src, Table, TimeOUt) ->
    db_handle_c:clear(Src, Table, TimeOUt).
