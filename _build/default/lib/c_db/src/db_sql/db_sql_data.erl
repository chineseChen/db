%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 17. 9月 2021 18:16
%%%-------------------------------------------------------------------
-module(db_sql_data).
-author("chenkecai").
-include("db_channel.hrl").

%% API
-export([erl_value_by_type/2]).
-export([get_data/2]).
-export([analyse/3, form/3, value2SqlVarByType/2, sql_value_by_type/2]).
-export([isNumberByType/1]).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
get_data({DBSrc, Tab}, Key) ->
    ModInfoT = dbT:get_mod(),
    {_, Mod} = o_tuple:get(Tab, ModInfoT),
    {ok, DBName} = gen_server:call('db_sql_mgr', {'get_db_name', DBSrc}),
    SqlStr = Mod:select(DBName),
    SqlPid = db_sql:getReadPid(),
    KeyIndex = Mod:key_index(),
    Index = ?BOOL_VALUE(is_tuple(KeyIndex), element(1, KeyIndex), KeyIndex),
    {_, ErlType, _, _} = Mod:info_by_index(Index),
    NKey = sql_value_by_type(Key, ErlType),
    {ok, ColumnT, DataL} = mysql:query(SqlPid, SqlStr, [NKey]),
    case DataL of
        [] -> none;
        _ -> analyse(Mod, ColumnT, DataL)
    end.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
form(Mod, Value, 'all') ->
    Count = size(Mod:init()),
    FieldNum = c_lib:for(fun(I, Acc) -> bit_lib:set_bit(Acc, I) end, 0, Count, 1), %%第一个是记录名
    form(Mod, Value, FieldNum);
form(Mod, Value, FieldNum) ->
    F = fun(Index, {Acc1, Acc2, Acc3, Acc4}) ->
        {Column, ErlType, _, _} = Mod:info_by_index(Index),
        Var = sql_value_by_type(element(Index, Value), ErlType),
        SetVar = io_lib:format("~s = values(~s)", [Column, Column]),
        NAcc4 = [SetVar | Acc4],
        {[binary_to_list(Column) | Acc1], [$,, $? | Acc2], [Var | Acc3], NAcc4}
    end,
    {ColumnL, [_ | Str0], VarL, SetVarL} = bit_lib:map(F, {[], [], [], []}, FieldNum),
    ColumnStr = string:join(lists:reverse(ColumnL), ","),
    SetVarStr = string:join(lists:reverse(SetVarL), ","),
    Str = io_lib:format("(~s) VALUES (~s)", [ColumnStr, Str0]),
    {Str, lists:reverse(VarL), SetVarStr}.

%%%===================================================================
%% @doc
%%  分析数据
%% @end
%%%===================================================================
analyse(Mod, ColumnT, DataL) ->
    analyse_(Mod, ColumnT, DataL, []).

analyse_(_Mod, _ColumnT, [], R) -> R;
analyse_(Mod, ColumnT, [H | T], R) ->
    Rec = Mod:init(),
    NRec = analyse_one(Mod, ColumnT, H, Rec),
    analyse_(Mod, ColumnT, T, [NRec | R]).

%%%===================================================================
%% @doc
%% 分析单个数据
%% @end
%%%===================================================================
analyse_one(_Mod, [], [], Rec) -> Rec;
analyse_one(Mod, [Column | T1], [Var | T2], Rec) ->
    case Mod:index_by_var(Column) of
        none -> analyse_one(Mod, T1, T2, Rec);
        Index when is_integer(Index) ->
            {_, Type, _, _} = Mod:info_by_index(Index),
            NVar = erl_value_by_type(Var, Type),
            NRec = erlang:setelement(Index, Rec, NVar),
            analyse_one(Mod, T1, T2, NRec)
    end.

%%%===================================================================
%% @doc
%%  数据转换
%% @end
%%%===================================================================
erl_value_by_type(Var, integer) -> Var;
erl_value_by_type(Var, float) -> Var;
erl_value_by_type(Var, string) ->
    unicode:characters_to_list(Var);
erl_value_by_type(Var, binary) -> Var;
erl_value_by_type(Var, _) ->
    str_lib:str_2_term(unicode:characters_to_list(Var)).

%%%===================================================================
%% @doc
%% 构告sql语句
%% @end
%%%===================================================================
sql_value_by_type(Var, integer) -> Var;
sql_value_by_type(Var, float) -> Var;
sql_value_by_type(Var, string) ->
    unicode:characters_to_binary(Var);
sql_value_by_type(Var, binary) -> Var;
sql_value_by_type(Var, _) -> str_lib:term_2_str(Var).

%%%===================================================================
%% @doc
%% 是否为数字
%% @end
%%%===================================================================
isNumberByType(integer) -> true;
isNumberByType(float) -> true;
isNumberByType(_) -> false.

%%%===================================================================
%% @doc
%% 构告sql语句
%% @end
%%%===================================================================
value2SqlVarByType(Var, integer) -> integer_to_binary(Var);
value2SqlVarByType(Var, float) -> float_to_binary(Var);
value2SqlVarByType(Var, string) ->
    Bin = unicode:characters_to_binary(Var),
    NBin = binary:replace(Bin, <<"'">>, <<"''">>, [global]),
    <<$', NBin/binary, $'>>;
value2SqlVarByType(Bin, binary) ->
    NBin = binary:replace(Bin, <<"'">>, <<"''">>, [global]),
    <<$', NBin/binary, $'>>;
value2SqlVarByType(Var, _) ->
    Bin = unicode:characters_to_binary(io_lib:format("~0p", [Var])),
    NBin = binary:replace(Bin, <<"'">>, <<"''">>, [global]),
    <<$', NBin/binary, $'>>.

