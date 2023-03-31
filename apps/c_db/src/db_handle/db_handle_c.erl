%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%  操作客户端,隐藏内部操作
%%% @end
%%% Created : 24. 11月 2021 15:58
%%%-------------------------------------------------------------------
-module(db_handle_c).
-author("chenkecai").
-define(FAULT_TOLERANT_TIME_M_SEC, 200).

%% API
-export([member/4]).
-export([get_all_k/3, get_values/4, get_count/3, get/5, m_get/5]).
-export([replace/5, replace_/5]).
-export([delete/4, delete_/4]).
-export([update_1/5, update_2/1, update_2/2]).
-export([handle_1/2, handle_2/1, handle_2/2, handle_2/3]).
-export([iterate/1, iterate_end/1]).
-export([clear/3]).
-export([is_tab/2]).

%%%===================================================================
%% @doc
%%  获取所有k
%% @end
%%%===================================================================
get_all_k(Src, Table, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    #{state := ok, data := Value} = gen_server:call('db_handle_proc',
        #{tab_keys => {Src, Table}, type => 'get_all_k', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    Value.

%%%===================================================================
%% @doc
%%  获取所有数据
%% @end
%%%===================================================================
get_values(Src, Table, Num, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    #{state := ok, data := Value} = gen_server:call('db_handle_proc',
        #{tab_keys => {{Src, Table}, Num}, type => 'get_values', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    Value.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
member(Src, Table, Key, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    #{state := ok, data := Value} = gen_server:call('db_handle_proc',
        #{tab_keys => {{Src, Table}, Key}, type => 'member', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    Value.

%%%===================================================================
%% @doc
%%  获取数据数量
%% @end
%%%===================================================================
get_count(Src, Table, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    #{state := ok, data := Value} = gen_server:call('db_handle_proc',
        #{tab_keys => {Src, Table}, type => 'table_count', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    Value.

%%%===================================================================
%% @doc
%%  获取数据
%% @end
%%%===================================================================
get(Src, Table, Key, Default, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    #{state := ok, data := [{_, Value}]} = gen_server:call('db_handle_proc',
        #{tab_keys => [{{Src, Table}, Key}], type => 'get', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    case Value of
        none -> Default;
        Value -> Value
    end.

%%%===================================================================
%% @doc
%% 获取多个数据
%% @end
%%%===================================================================
m_get(_Src, _Table, [], _Default, _TimeOut) -> [];
m_get(Src, Table, Keys, Default, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    TabKeys = [{{Src, Table}, Key} || Key <- Keys],
    #{state := ok, data := DataL} = gen_server:call('db_handle_proc',
        #{tab_keys => TabKeys, type => 'get', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    [case Value of
        none -> Default;
        Value -> Value
    end || {_, Value} <- DataL].

%%%===================================================================
%% @doc
%%  更新数据
%% @end
%%%===================================================================
replace(Src, Table, Key, Value, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    check_data(Table, Key, Value),
    TabKey = {{Src, Table}, Key},
    #{state := ok} = gen_server:call('db_handle_proc',
        #{tab_keys => [{TabKey, Value}], type => 'replace', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    ok.

%%%===================================================================
%% @doc
%%  更新数据,返回old数据
%% @end
%%%===================================================================
replace_(Src, Table, Key, Value, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    check_data(Table, Key, Value),
    TabKey = {{Src, Table}, Key},
    #{state := ok, data := [{_, OldValue}]} = gen_server:call('db_handle_proc',
        #{tab_keys => [{TabKey, Value}], type => 'replace', time_out => TimeOut, info => 'old_data_flag'}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    {ok, OldValue}.

%%%===================================================================
%% @doc
%% 删除数据
%% @end
%%%===================================================================
delete(Src, Table, Key, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    TabKey = {{Src, Table}, Key},
    #{state := ok} = gen_server:call('db_handle_proc',
        #{tab_keys => [{TabKey, 'delete'}], type => 'replace', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    ok.

%%%===================================================================
%% @doc
%% 删除数据
%% @end
%%%===================================================================
delete_(Src, Table, Key, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    TabKey = {{Src, Table}, Key},
    #{state := ok, data := [{_, OldValue}]} = gen_server:call('db_handle_proc',
        #{tab_keys => [{TabKey, 'delete'}], type => 'replace', time_out => TimeOut, info => 'old_data_flag'}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    OldValue.

%%%===================================================================
%% @doc
%%  update 第一步
%% @end
%%%===================================================================
update_1(Src, Table, Key, Default, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    ResMap = #{state := ok, data := [{_, Value}]} = gen_server:call('db_handle_proc',
        #{tab_keys => [{{Src, Table}, Key}], type => 'update', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    RelValue = case Value of
        none -> Default;
        _ -> Value
    end,
    {ok, RelValue, ResMap}.

%%%===================================================================
%% @doc
%%  update 第二步
%% @end
%%%===================================================================
update_2(ResMap) ->
    #{state := ok} = gen_server:call('db_handle_proc', maps:remove('data', ResMap)).
update_2(ResMap, NValue) ->
    #{data := [{{{_, Table}, Key} = TabKey, OldValue}]} = ResMap,
    case NValue =:= OldValue orelse (OldValue =:= none andalso NValue =:= 'delete') of
        true -> #{state := ok} = gen_server:call('db_handle_proc', maps:remove('data', ResMap));
        false ->
            true = check_data(Table, Key, NValue),
            #{state := ok} = gen_server:call('db_handle_proc', ResMap#{'data' := [{TabKey, NValue}]})
    end.

%%%===================================================================
%% @doc
%%  事务操作数据第一步
%% @end
%%%===================================================================
handle_1([_ | _] = TabKeys, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    F1 = fun
        ({Src, Tab, Key}, {Acc1, Acc2, Acc3}) ->
            is_tab(Src, Tab),
            TabKey = {{Src, Tab}, Key},
            [error({"tab key repeat", TabKeys}) || lists:member(TabKey, Acc3)],
            {[TabKey | Acc1], [{TabKey, none} | Acc2], [TabKey | Acc3]};
        ({Src, Tab, Key, Value}, {Acc1, Acc2, Acc3}) ->
            is_tab(Src, Tab),
            TabKey = {{Src, Tab}, Key},
            [error({"tab key repeat", TabKeys}) || lists:member(TabKey, Acc3)],
            {[TabKey | Acc1], [{TabKey, Value} | Acc2], [TabKey | Acc3]}
    end,
    {NTabKeyL, DefaultL, _} = lists:foldl(F1, {[], [], []}, TabKeys),
    ResMap = #{state := ok, data := DataL0} = gen_server:call('db_handle_proc',
        #{tab_keys => NTabKeyL, type => 'handle', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    F2 = fun(_, {[{TabKey1, Value} | Acc1], [{TabKey2, Default} | Acc2], R}) when TabKey1 =:= TabKey2 ->
        case Value of
            none -> {Acc1, Acc2, [{TabKey1, Default} | R]};
            _ -> {Acc1, Acc2, [{TabKey1, Value} | R]}
        end
    end,
    {_, _, DataL} = c_lib:for(F2, {DataL0, DefaultL, []}, 0, length(TabKeys)),
    {ok, DataL, ResMap#{tab_keys => NTabKeyL}}.

%%%===================================================================
%% @doc
%%  事务操作数据第二步
%% @end
%%%===================================================================
handle_2(ResMap) ->
    #{state := ok} = gen_server:call('db_handle_proc', maps:remove('data', ResMap)).
handle_2(ResMap, NDataL) ->
    handle_2(ResMap, NDataL, []).
handle_2(#{tab_keys := TabKeyL, data := ODataL} = ResMap, NDataL0, IDataL) ->
    checkHandleData(TabKeyL, NDataL0),
    NIDataL = analysis_insert_data(TabKeyL, IDataL),
    FilterF = fun({K, NV}) ->
        {_, OldV} = lists:keyfind(K, 1, ODataL),
        not (OldV =:= NV orelse (NV =:= 'delete' andalso OldV =:= none))
    end,
    NDataL = lists:filter(FilterF, NDataL0),
    ResMap1 = case NDataL of
        [] -> maps:remove('data', ResMap);
        _ -> ResMap#{'data' := NDataL}
    end,
    ResMap2 = case NIDataL of
        [] -> ResMap1;
        _ -> ResMap1#{'insert_data' => NIDataL}
    end,
    #{state := ok} = gen_server:call('db_handle_proc', ResMap2).

%%%===================================================================
%% @doc
%%  清理表
%% @end
%%%===================================================================
clear(Src, Table, TimeOut) when is_integer(TimeOut) andalso TimeOut > 0 ->
    is_tab(Src, Table),
    #{state := ok} = gen_server:call('db_handle_proc',
        #{tab_keys => {Src, Table}, type => 'clear', time_out => TimeOut}, TimeOut + ?FAULT_TOLERANT_TIME_M_SEC),
    ok.

%%%===================================================================
%% @doc
%%  执行遍历
%% @end
%%%===================================================================
iterate(#{time_out := TimeOut} = Map) when is_integer(TimeOut) andalso TimeOut > 0 ->
    NMap = #{state := 'ok', key := Key} = gen_server:call('db_handle_proc', Map),
    {ok, NMap#{time_out => TimeOut}, Key}.

%%%===================================================================
%% @doc
%%  遍历释放
%% @end
%%%===================================================================
iterate_end(NMap) ->
    #{state := 'ok'} = gen_server:call('db_handle_proc', NMap).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
checkHandleData(TabKeyL, NDataL) ->
    c_lib:foreach(
        fun({{{_, Table}, Key} = SKey, Value}, SubTabKeyL) ->
            check_data(Table, Key, Value),
            case lists:member(SKey, SubTabKeyL) of
                false -> error({SKey, SubTabKeyL, "data error"});
                true -> lists:delete(SKey, SubTabKeyL)
            end
        end, TabKeyL, NDataL).

%%%===================================================================
%% @doc
%% 检查插入数据
%% @end
%%%===================================================================
analysis_insert_data(TabKeyL, InsertDataL) ->
    F = fun({Src, Table, Key, Value}, {Acc1, Acc2}) ->
        is_tab(Src, Table),
        check_data(Table, Key, Value),
        [error("insert data can not delete") || Value =:= 'delete'],
        SKey = {{Src, Table}, Key},
        [error({SKey, TabKeyL, "insert data error"}) || lists:member(SKey, TabKeyL)],
        [error({SKey, Acc2, "insert data error"}) || lists:member(SKey, Acc1)],
        {[SKey || Acc1], [{SKey, Value} | Acc2]}
    end,
    {_, IDataL} = c_lib:foreach(F, {[], []}, InsertDataL),
    IDataL.


%%%===================================================================
%% @doc
%%  是否是表,系统表不让操作
%% @end
%%%===================================================================
is_tab(Src, Table) ->
    {ok, DbSrcL} = c_db_app:get_db_src(),
    [error({Src, Table, "table error"}) || not lists:member(Src, DbSrcL)],
    T = dbT:get_mod(),
    case o_tuple:get(Table, T) of
        false -> error({Src, Table, "table error"});
        {_, L} when is_list(L) ->
            [error({Src, Table, "table error"}) || not lists:member(Src, L)];
        {_, Mod} ->
            [error({Src, Table, "table error"}) || not lists:member(Src, Mod:db_src())]
    end.

%%%===================================================================
%% @doc
%%  检查据是否合法,mysql数据必须是record
%% @end
%%%===================================================================
check_data(_Table, _Key, 'delete') -> true;
check_data(_Table, 'none' = Key, _) ->
    error({Key, "key is system key"});
check_data(_Table, '$end_of_table' = Key, _) ->
    error({Key, "key is system key"});
check_data(Table, Key, Data) ->
    T = dbT:get_mod(),
    case o_tuple:get(Table, T) of
        {_, Mod} when is_atom(Mod) ->
            KeyIndex = Mod:key_index(),
            case is_tuple(KeyIndex) of
                true ->
                    checkMulData(Mod, KeyIndex, Key, Data);
                false ->
                    [error({Data, "data not tuple"}) || not is_tuple(Data)],
                    [error({Data, "data key error1"}) || Key =/= element(Mod:key_index(), Data)],
                    [error({Data, "data element type error"}) || not Mod:check(Data)]
            end,
            true;
        _ ->
            [error({Data, "data not tuple"}) || is_tuple(Data)],
            [error({Data, "data key error2"}) || Key =:= element(1, Data)],
            true
    end.

%%%===================================================================
%% @doc
%% 检查多键数据
%% @end
%%%===================================================================
checkMulData(_Mod, _KeyIndex, _Key, 'delete') -> ok;
checkMulData(Mod, KeyIndex, Key, DataL) ->
    CheckF = fun(Value) ->
        case is_tuple(Value) of
            true ->
%%                io:format("mod:~p line:~p ==============arg:~p~n", [?MODULE, ?LINE, element(element(1, KeyIndex), Value) =:= Key ]),
                case element(element(1, KeyIndex), Value) =:= Key of
                    true -> [error({Value, "data element type error"}) || not Mod:check(Value)];
                    false -> error({Key, Value, "data key error"})
                end;
            false ->
                error({Value, "data error"})
        end
    end,
    lists:foreach(CheckF, DataL).