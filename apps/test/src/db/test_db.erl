%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 07. 9月 2021 15:55
%%%-------------------------------------------------------------------
-module(test_db).
-author("chenkecai").
%% API
-export([do/0, handle/0, handle/3, handle_timer/0, handle_timer/1, handle_m_timer/1, update/1, iterate/0, update_1/1, new_paste_id/0, when_test/1]).
-export([t_get/1]).
-export([etsM/1]).

do() ->
%%    select COLUMN_NAME,ORDINAL_POSITION,COLUMN_DEFAULT,DATA_TYPE,COLUMN_COMMENT,COLUMN_TYPE  from information_schema.columns where table_name = 'test_db';
    {ok, Pid} = mysql:start_link([
        {ip, "127.0.0.1"},
        {port, 3306},
        {user, "chenkecai"},
        {password, "ckc123456"},
        {database, "my_pro"}
    ]),
    Res = mysql:query(Pid, <<"INSERT INTO test1 (id1) VALUES (?)">>, [2]),
    mysql:stop(Pid),
    Res.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
handle() ->
%%    Id1 = rand:uniform(20),
%%    Id2 = rand:uniform(20),
    handle(rand_lib:between(1, 100), rand_lib:between(1, 16#fffffff), rand:uniform(16#fffffff)).
%%    handle(rand_lib:between(1,2), rand:uniform(16#fffffff), 1).
%%handle(Id1, Id2, Id3) ->
%%    Var2 = [dbT_test3:init(Id2, Value) || Value <- lists:seq(1, rand_lib:between(10, 200))],
%%    Var3 = [dbT_test4:init(Id3, Value) || Value <- lists:seq(1, rand_lib:between(10, 200))],
%%    F = fun(_, [{Index1, Var1}, {Index2, _Var2}, {Index3, _Var3}]) ->
%%        {ok, ok, [
%%            {Index1, dbT_test:set_key1(dbT_test:set_var3(Var1, integer_to_binary(rand:uniform(16#ffffff))), dbT_test:get_key1(Var1) + 1)},
%%            {Index2, Var2},
%%            {Index3, Var3}
%%        ]}
%%    end,
%%    TabKeys = [
%%        {game, test, Id1, dbT_test:init(Id1)},
%%        {game, test3, Id2, []},
%%        {game, test4, Id3, []}
%%    ],
%%    catch c_db_lib:handle(TabKeys, F, []).
handle(Id1, Id2, Id3) ->
    L = lists:seq(1, 1000),
    Bin = <<<<"t">> || _ <- L>>,
    F = fun(_, [{Index1, _}, {Index2, _}, {Index3, _}]) ->
        T1 = dbT_test1:init(Id3),
        {ok, ok, [
            {Index1, dbT_test:set_var3(dbT_test:init(Id1), integer_to_binary(rand:uniform(16#ffffff)))},
            {Index2, [dbT_test3:init(Id2, rand:uniform(16#ffffff))]},
            {Index3, dbT_test1:set_key1(T1, Bin)}]}
    end,
    TabKeys = [
        {game, test, Id1, dbT_test:init(Id1)},
        {game, test3, Id2, []},
        {game, test1, Id3, []}
    ],
    catch c_db_lib:handle(TabKeys, F, []).

handle_timer() ->
    handle_timer(self()).
handle_timer(Pid) ->
    {T, _} = Res = timer:tc(fun handle/0),
    Pid ! Res,
    T.

handle_m_timer(Count) ->
    L = lists:seq(1, Count),
    spawn(fun() ->
        Pid = self(),
        put('length', {0, 0}),
        [spawn(fun() -> handle_timer(Pid) end) || _ <- L],
        F = fun(_I, Acc) ->
            receive {T, Res} ->
                {SL, FL} = get('length'),
                case Res =:= ok of
                    true -> put('length', {SL + 1, FL}), Acc + T;
                    _ -> put('length', {SL, FL + 1}), Acc
                end
            after 10000 ->
                {break, Acc}
            end
        end,
        {SumT, _Sum} = timer:tc(fun() -> c_lib:for(F, 0, 1, Count + 1) end),
        {SL, FL} = get('length'),
        io:format("module:~p line:~p==================arg:~p~n", [?MODULE, ?LINE, {SumT, [SumT / SL || SL > 0], SL, FL, SL + FL}]),
%%        [begin
%%            ChildL = supervisor:which_children(db_lock_queue_sup),
%%            io:format("module:~p line:~p==================arg:~p~n", [?MODULE, ?LINE, ChildL]),
%%            F2 = fun(Arg) ->
%%                CPid = element(2, Arg),
%%                R = catch length(gen_server:call(CPid, 'get_all_data')),
%%                io:format("module:~p line:~p==================arg:~p~n", [?MODULE, ?LINE, R]),
%%                ok
%%            end,
%%            [F2(Arg) || Arg <- ChildL]
%%        end || FL > 0],
%%        Sum = lists:foldl(F, 0, L),
        ok
    end),
    ok.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
t_get(Num) ->
    Pid1 = self(),
    Pid2 = spawn_link(fun() -> get_count(0, Num, Pid1) end),
    F = fun(_, Acc) ->
        [spawn(fun() -> get_(Pid2) end) | Acc]
    end,
    c_lib:for(F, [], 0, Num),
    receive
        R ->
            io:format("mod:~p line:~p ==============arg:~p~n", [?MODULE, ?LINE, R])
    after 60000 ->
        io:format("mod:~p line:~p ==============arg:~p~n", [?MODULE, ?LINE, time_out])
    end,
    ok.

get_count(Num, Max, Pid1) ->
    receive ok ->
        NNum = Num + 1,
        case NNum >= Max of
            true -> Pid1 ! {ok, NNum};
            false ->
                get_count(NNum, Max, Pid1)
        end
    after
        5000 ->
            Pid1 ! {error, Num}
    end.

get_(Pid) ->
    c_db_lib:get(gm, test1, rand:uniform(16#fffffff), none),
    Pid ! ok,
    receive
    after 5000 ->
        Pid ! error
    end.


when_test(Value) ->
    case true of
        true when Value ->
            ok1;
        true when element(1, Value) =:= 0 ->
            ok2;
        _ -> error
    end.
%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
update(Key) ->
    F = fun(_, Value) ->
        dbT_test:get_var1(Value),
        {ok, ok, dbT_test:set_var2(Value, "''")}
    end,
    c_db_lib:update(game, test, Key, F, dbT_test:init(Key)).

%%%===================================================================
%% @doc
%%  遍历测试
%% @end
%%%===================================================================
iterate() ->
    io:format("module:~p line:~p ==============arg:~p~n", [?MODULE, ?LINE, c_db_lib:get_count(gms, test1)]),
    F = fun(_, Key, Arg) ->
        {ok, [c_db_lib:get(gms, test1, Key) | Arg]}
    end,
    L = c_db_lib:iterate(gms, test1, F, []),
    io:format("module:~p line:~p ==============arg:~p~n", [?MODULE, ?LINE, {length(L), begin [A, B | _] = lists:reverse(L), {A, B} end}]),
    ok.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
update_1(Key) ->
    SpawnF = fun() ->
        F1 = fun(_, _) ->
            Value = db_t_test:init(Key),
            NValue = db_t_test:set_var2(Value, io_lib:format("test~p", [Key])),
            timer:sleep(2000),
            {ok, ok, NValue}
        end,
        c_db_lib:update(test, Key, F1)
    end,
    spawn(SpawnF),
    timer:sleep(1000),
    F2 = fun(_, Value) ->
        io:format("module:~p line:~p==================arg:~p~n", [?MODULE, ?LINE, Value]),
        NValue = db_t_test:set_var2(Value, io_lib:format("test~p", [Key + 1])),
        {ok, NValue, NValue}
    end,
    Var1 = c_db_lib:update(test, Key, F2),
    Var2 = c_db_lib:get(test, Key),
    io:format("module:~p line:~p==================arg:~p~n", [?MODULE, ?LINE, {Var1, Var2}]),
    ok.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
new_paste_id() ->
    Initial = rand:uniform(62) - 1,
    new_paste_id(<<Initial>>, 7).
new_paste_id(Bin, 0) ->
    Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
    <<<<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin>>;
new_paste_id(Bin, Rem) ->
    Next = rand:uniform(62) - 1,
    new_paste_id(<<Bin/binary, Next>>, Rem - 1).

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
etsM(N) ->
    Name = '$xxtest',
    ets:new(Name, [set, protected, named_table, {keypos, 1}]),
    L = [{V, rand:uniform(1000)} || V <- lists:seq(1, N)],
    ets:insert(Name, L),
    {_, C} = ets:match_object(Name, '_', 1),
    ets:delete_all_objects(Name),
    X = ets:tab2list(Name),
    F = fun(_, CC) ->
        case ets:match_object(CC) of
            '$end_of_table' -> {break, ok};
            {_, Con} ->
                Con
        end
    end,
    c_lib:for(F, C, 0, N),
    ets:delete(Name),
    X.