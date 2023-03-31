%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 09. 9月 2021 14:03
%%%-------------------------------------------------------------------
-module(db_2_file).
-author("chenkecai").
-define(C_DB_FILE_TAG, 'c_db').
-define(C_BI_FILE_TAG, 'c_bi').
-define(BOOL_VALUE(Bool, Value1, Value2), case Bool of true -> Value1; false -> Value2 end).

%% API
-export([do/0, remove_file/0]).

%%%===================================================================
%% @doc
%%  重新执行
%% @end
%%%===================================================================
remove_file() ->
    remove_file(?C_DB_FILE_TAG),
    remove_file(?C_BI_FILE_TAG),
    ok.
remove_file(DirName) ->
    {ok, NowPath} = file:get_cwd(),
    Dir = lists:flatten(io_lib:format("~s/apps/~s", [NowPath, DirName])),
    [begin
        Dir1 = lists:flatten(io_lib:format("~s/out/src", [Dir])),
        ?BOOL_VALUE(filelib:is_dir(Dir1), begin
            file:del_dir_r(Dir1),
            file:make_dir(Dir1)
        end, file:make_dir(Dir1)),
        Dir2 = lists:flatten(io_lib:format("~s/out/include", [Dir])),
        ?BOOL_VALUE(filelib:is_dir(Dir2), begin
            file:del_dir_r(Dir2),
            file:make_dir(Dir2)
        end, file:make_dir(Dir2))
    end || filelib:is_dir(Dir)].

%%%===================================================================
%% @doc
%%  执行db cfg转配制文件
%% @end
%%%===================================================================
do() ->
    {ok, NowPath} = file:get_cwd(),
    do_all(NowPath, ?C_DB_FILE_TAG),
    do_all(NowPath, ?C_BI_FILE_TAG),
    ok.
do_all(NowPath, FileTag) ->
    PathL = get_cfg_file(FileTag, NowPath),
    do_from_file(NowPath, FileTag, PathL).

do_from_file(_NowPath, _FileTag, []) -> ok;
do_from_file(NowPath, FileTag, PathL) ->
    F = fun(Path, R) -> analysis(NowPath, FileTag, Path, R) end,
    {Bool1, FileInfoL} = c_lib:foreach(F, {false, []}, PathL),
    Bool2 = del_file(FileInfoL, NowPath, FileTag),
    [begin
        File = case FileTag of
            ?C_DB_FILE_TAG ->
                lists:flatten(io_lib:format("~s/apps/~s/out/src/dbT/dbT.erl", [NowPath, FileTag]));
            ?C_BI_FILE_TAG ->
                lists:flatten(io_lib:format("~s/apps/~s/out/src/biT/biT.erl", [NowPath, FileTag]))
        end,
        FileDir = filename:dirname(File),
        [file:make_dir(FileDir) || not filelib:is_dir(FileDir)],
        {ok, IoDevice} = file:open(File, [write, {encoding, utf8}]),
        HStr = total_head_str(FileTag),
        {FModStr, EModStr} = mod_func(FileInfoL),
        {FMd5Str, EMd5Str} = md5_func(FileInfoL),
        {FFileStr, EFileStr} = file_func(FileInfoL),
        io:put_chars(IoDevice, HStr),
        io:put_chars(IoDevice, export_func([EModStr, EMd5Str, EFileStr])),
        io:put_chars(IoDevice, "\r\n"),
        io:put_chars(IoDevice, FModStr),
        io:put_chars(IoDevice, FMd5Str),
        io:put_chars(IoDevice, FFileStr),
        file:close(IoDevice)
    end || Bool1 orelse Bool2].

%%%===================================================================
%% @doc
%%  执行因配制数据删除而删除文件
%% @end
%%%===================================================================
del_file(FileInfoL, NowPath, ?C_DB_FILE_TAG) ->
    del_file(FileInfoL, NowPath, ?C_DB_FILE_TAG, dbT);
del_file(FileInfoL, NowPath, ?C_BI_FILE_TAG) ->
    del_file(FileInfoL, NowPath, ?C_BI_FILE_TAG, biT).
del_file(FileInfoL, NowPath, FileFlag, MMod) ->
    case erlang:function_exported(MMod, get_mod, 0) of
        true ->
            TagModL = tuple_to_list(MMod:get_mod()),
            SubTagModL = lists:filter(fun({Tag, _Mod}) -> not lists:keymember(Tag, 2, FileInfoL) end, TagModL),
            F1 = fun(Tag) ->
                ErlFile = MMod:get_file(Tag),
                file:delete(ErlFile),
                ErlDirName = filename:dirname(ErlFile),
                [file:del_dir(ErlDirName) || file:list_dir(ErlDirName) =:= {ok, []}]
            end,
            F2 = fun(Mod) ->
                HrlFile = lists:flatten(io_lib:format("~s/apps/~s/out/include/~s.hrl", [NowPath, FileFlag, Mod])),
                file:delete(HrlFile)
            end,
            [begin F1(Tag), F2(Mod) end || {Tag, Mod} <- SubTagModL, is_atom(Mod)],
            SubTagModL =/= [];
        false -> true
    end.

%%%===================================================================
%% @doc
%% 获取配制文件路径
%% @end
%%%===================================================================
get_cfg_file(?C_BI_FILE_TAG, NowPath) ->
    Path = NowPath ++ "/config/db/bi/*_bi.cfg",
    filelib:wildcard(Path);
get_cfg_file(?C_DB_FILE_TAG, NowPath) ->
    Path1 = NowPath ++ "/config/db/main/*_db.cfg",
    Path2 = NowPath ++ "/config/db/main/sys/*_db.cfg",
    filelib:wildcard(Path1) ++ filelib:wildcard(Path2).

%%%===================================================================
%% @doc
%% 分析每一个文件
%% @end
%%%===================================================================
analysis(NowPath, ?C_DB_FILE_TAG, Path, FInfo) ->
    Md5Bool = erlang:function_exported(dbT, get_md5, 1),
    PreModStr = "dbT_",
    analysis_(NowPath, ?C_DB_FILE_TAG, Path, FInfo, Md5Bool, PreModStr, dbT);
analysis(NowPath, ?C_BI_FILE_TAG, Path, FInfo) ->
    Md5Bool = erlang:function_exported(biT, get_md5, 1),
    PreModStr = "biT_",
    analysis_(NowPath, ?C_BI_FILE_TAG, Path, FInfo, Md5Bool, PreModStr, biT).

%%%===================================================================
%% @doc
%% 分析每一个文件
%% @end
%%%===================================================================
analysis_(NowPath, FileTag, Path, FInfo, Md5Bool, PreModStr, MMod) ->
    {ok, CfgL} = file:consult(Path),
    SupFName = filename:rootname(filename:basename(Path)),
    F = fun({Tag, DbType, DBSrc, ColumnL, _InfoL} = Cfg, {Bool, L}) ->
        Str = str_lib:term_2_str(Cfg),
        Md5 = c_lib:md5_hex(Str),
        ModStr = PreModStr ++ atom_to_list(Tag),
        Mod = list_to_atom(ModStr),
        [error({Mod, " table repeat"}) || lists:keymember(Mod, 1, L)],
        %%相对路径,每个项目的工作目录不一样
        EFile = lists:flatten(io_lib:format("./apps/~s/out/src/~s/~s.erl", [FileTag, SupFName, ModStr])),
        ChangeR = check_change(not Md5Bool orelse Md5 =/= MMod:get_md5(Tag), ColumnL, fun() ->
            creat_file(NowPath, FileTag, SupFName, ModStr)
        end),
        if
            ChangeR =:= 'no_change' andalso ColumnL =/= [] ->
                {Bool, [{Mod, Tag, Md5, EFile} | L]};
            ChangeR =:= 'no_change' ->
                DBSrcL = c_lib:get_bool_value(is_atom(DBSrc), [DBSrc], DBSrc),
                {Bool, [{DBSrcL, Tag, Md5, EFile} | L]};
            ChangeR =:= 'no_data' andalso FileTag =:= ?C_BI_FILE_TAG ->
                error("bi config must be not null");
            FileTag =:= ?C_BI_FILE_TAG andalso DbType =/= 'disk' ->
                error("bi config type must be disk");
            ChangeR =:= 'no_data' andalso Md5Bool ->
                DBSrcL = c_lib:get_bool_value(is_atom(DBSrc), [DBSrc], DBSrc),
                {true, [{DBSrcL, Tag, none, none} | L]};
            ChangeR =:= 'no_data' ->
                DBSrcL = c_lib:get_bool_value(is_atom(DBSrc), [DBSrc], DBSrc),
                case Md5Bool of
                    true ->
                        case o_tuple:get(Tag, MMod:get_mod()) of
                            {_, DBSrcL} -> {Bool, [{DBSrcL, Tag, none, none} | L]};
                            _ -> {true, [{DBSrcL, Tag, none, none} | L]}
                        end;
                    false -> {true, [{DBSrcL, Tag, none, none} | L]}
                end;
            true ->
                {EFd, HFd} = ChangeR,
                write_2_file(ModStr, Cfg, EFd, HFd),
                {true, [{Mod, Tag, Md5, EFile} | L]}
        end
    end,
    c_lib:foreach(F, FInfo, CfgL).

%%%===================================================================
%% @doc
%% 文件变化
%% @end
%%%===================================================================
check_change(true, [], _) -> no_data;
check_change(true, [_ | _], F) -> F();
check_change(false, _, _) -> no_change.

%%%===================================================================
%% @doc
%%  创建文件
%% @end
%%%===================================================================
creat_file(NowPath, DirName, SupFName, ModStr) ->
    DirPath = lists:flatten(io_lib:format("~s/apps/~s/out/src/~s/", [NowPath, DirName, SupFName])),
    [file:make_dir(DirPath) || not filelib:is_dir(DirPath)],
    EFile = lists:flatten(io_lib:format("~s~s.erl", [DirPath, ModStr])),
    HFile = lists:flatten(io_lib:format("~s/apps/~s/out/include/~s.hrl", [NowPath, DirName, ModStr])),
    {ok, EFd0} = file:open(EFile, [write, {encoding, utf8}]),
    {ok, HFd0} = file:open(HFile, [write, {encoding, utf8}]),
    {EFd0, HFd0}.

%%%===================================================================
%% @doc
%%  写内容至文件
%% @end
%%%===================================================================
write_2_file(ModStr, Cfg, EFd, HFd) ->
    {Tag, DbType, DBSrc, ColumnL, InfoL} = Cfg,
    HStr = head_str(ModStr, InfoL),
    io:put_chars(EFd, HStr),
    TagStr = atom_to_list(Tag),
    #{rec_str := RecStr, func_get_str := {GetStr, GetEStr}, func_set_str := {SetStr, SetEStr}, check_func_str := {CheckStr, CheckEStr}} =
        CResult = column_str(ColumnL, #{tag_str => TagStr, type => DbType, info => {hd(ColumnL), InfoL}}),
    io:put_chars(HFd, RecStr),
    file:close(HFd),
    {KeyInFStr, KeyInEStr, KeyColumn} = key_index(ColumnL, InfoL),
    {InitFStr, InitEStr} = func_init_str(TagStr, KeyColumn),
    {DBSrcFStr, DBSrcEStr} = func_db_src(DBSrc),
    {SaveTypeFStr, SaveTypeEStr} = save_type_func(DbType),
    io:put_chars(EFd, export_func([InitEStr, DBSrcEStr, KeyInEStr, CheckEStr])),
    io:put_chars(EFd, GetEStr),
    io:put_chars(EFd, SetEStr),
    io:put_chars(EFd, SaveTypeEStr),
    FF = fun() ->
        io:put_chars(EFd, "\r\n"),
        io:put_chars(EFd, InitFStr),
        io:put_chars(EFd, DBSrcFStr),
        io:put_chars(EFd, KeyInFStr),
        io:put_chars(EFd, CheckStr),
        io:put_chars(EFd, GetStr),
        io:put_chars(EFd, SetStr),
        io:put_chars(EFd, SaveTypeFStr)
    end,
    case DbType =:= 'disk' of
        true ->
            #{'func_index_str' := {IndexStr, IndexEStr}, 'func_var_str' := {VarFStr, VarEStr},
                'func_create_sql' := {CreateFunStr, CreateEStr}} = CResult,
            {SelFStr, SelEStr} = func_select(TagStr, CResult),
            {DelFStr, DelEStr} = func_delete(TagStr, CResult),
            {PriKeyFStr, PriKeyEStr} = primary_key_str(CResult),
            {TabNameStr, TabNameEStr} = sql_table_str(TagStr),
            io:put_chars(EFd, export_func([IndexEStr, VarEStr])),
            io:put_chars(EFd, export_func([TabNameEStr, PriKeyEStr, CreateEStr, SelEStr, DelEStr])),
            FF(),
            io:put_chars(EFd, IndexStr),
            io:put_chars(EFd, VarFStr),
            io:put_chars(EFd, CreateFunStr),
            io:put_chars(EFd, TabNameStr),
            io:put_chars(EFd, PriKeyFStr),
            io:put_chars(EFd, SelFStr),
            io:put_chars(EFd, DelFStr);
        false ->
            FF()
    end,
    file:close(EFd).

%%%===================================================================
%% @doc
%%  mod文件头部
%% @end
%%%===================================================================
head_str(Mod, InfoL) ->
    CommentStr = proplists:get_value('comment', InfoL, ""),
    "%%%==================================================================="
    "\r\n%% @doc"
    "\r\n%%\t" ++ CommentStr ++ "\r\n%% @end"
    "\r\n%%%==================================================================="
    "\r\n-module(" ++ Mod ++ ")\.\r\n"
    "-author(sql)\.\r\n"
    "-include(\"" ++ Mod ++ ".hrl\").\r\n"
    "\r\n".

%%%===================================================================
%% @doc
%% init函数字符串
%% @end
%%%===================================================================
func_init_str(TagStr, [_ | _] = KeyColumn) ->
    MaxIndex = length(KeyColumn),
    F = fun(Column, {Index, AAcc, BAcc, CAcc} = R) ->
        RecKey = str_lib:term_2_str(element(1, Column)),
        RecKeyType = element(2, Column),
        TypeStr1 = str_lib:term_2_str(?BOOL_VALUE(RecKeyType =:= 'string', 'list', RecKeyType)),
        ?BOOL_VALUE(TypeStr1 =:= "term", R, begin
            KeyVar = "Key" ++ integer_to_list(Index),
            ConStr = "is_" ++ TypeStr1 ++ "(" ++ KeyVar ++ ")",
            {Index - 1, [KeyVar | AAcc], [ConStr | BAcc], [RecKey ++ " = " ++ KeyVar | CAcc]}
        end)
    end,
    {_, StrL1, StrL2, StrL3} = lists:foldr(F, {MaxIndex, [], [], []}, KeyColumn),
    ArgStr = string:join(StrL1, ", "),
    WhenStr = string:join(StrL2, " andalso "),
    RecKvStr = string:join(StrL3, ","),
    {"init() ->\r\n\t #" ++ TagStr ++ "{}.\r\n"
    "init(" ++ ArgStr ++ ") when " ++ WhenStr ++ "->\r\n\t #" ++ TagStr ++ "{" ++ RecKvStr ++ "}.\r\n\r\n",
            "init/0, init/" ++ integer_to_list(length(StrL1))};
func_init_str(TagStr, KeyColumn) ->
    func_init_str(TagStr, [KeyColumn]).

%%%===================================================================
%% @doc
%%  数据库源
%% @end
%%%===================================================================
func_db_src(DBSrc) when is_atom(DBSrc) ->
    func_db_src([DBSrc]);
func_db_src(DBSrc) ->
    {"db_src() ->\r\n\t" ++ str_lib:term_2_str(DBSrc) ++ ".\r\n\r\n", "db_src/0"}.

%%%===================================================================
%% @doc
%%  单个元素分析
%% @end
%%%===================================================================
column_str(ColumnL, R) ->
    column_str(ColumnL, 2, R).

column_str([{Name, EType, SqlType, Default, Comment} | T], Index, R) ->
    column_str([{Name, EType, SqlType, Default, Comment, []} | T], Index, R);
column_str([{Name, EType, SqlType, Default, Comment, Info}], Index, R) ->
    [check(EType, SqlType) || maps:get(type, R, none) =:= disk],
    NameStr = str_lib:term_2_str(Name),
    ETypeStr = str_lib:term_2_str(EType),
    NR0 = record_str_2(NameStr, ETypeStr, Default, Comment, R),
    #{'tag_str' := TagStr} = R,
    NR1 = func_rec_str_2('func_get_str', fun() -> func_get_str(TagStr, NameStr) end, NR0),
    NR2 = func_rec_str_2('func_set_str', fun() -> func_set_str(TagStr, NameStr, ETypeStr) end, NR1),
    NR3 = func_check_str_2(TagStr, NameStr, ETypeStr, NR2),
    case maps:get('type', R) of
        'disk' ->
            NameStr1 = io_lib:format("`~s`", [Name]),

            NR4 = func_rec_str_1('func_index_str', fun() ->
                func_index(Index, NameStr1, EType, SqlType, Comment) end, NR3),
            NR5 = func_rec_str_1('func_var_str', fun() -> func_var(Index, NameStr) end, NR4),

            NR6 = func_rec_str_2('func_index_str', fun() -> func_last_index() end, NR5, false),
            NR7 = func_rec_str_2('func_var_str', fun() -> func_last_var() end, NR6, false),
            NR8 = func_create_1(Name, EType, SqlType, Default, Comment, Info, NR7),
            func_create_2(NR8);
        _ -> NR3
    end;
column_str([{Name, EType, SqlType, Default, Comment, Info} | T], Index, R) ->
    [check(EType, SqlType) || maps:get(type, R, none) =:= disk],
    NameStr = str_lib:term_2_str(Name),
    ETypeStr = str_lib:term_2_str(EType),
    NR0 = record_str_1(NameStr, ETypeStr, Default, Comment, R),
    #{'tag_str' := TagStr} = R,
    NR1 = func_rec_str_1('func_get_str', fun() -> func_get_str(TagStr, NameStr) end, NR0),
    NR2 = func_rec_str_1('func_set_str', fun() -> func_set_str(TagStr, NameStr, ETypeStr) end, NR1),
    NR3 = func_check_str_1(TagStr, NameStr, ETypeStr, NR2),
    case maps:get('type', R) of
        'disk' ->
            NameStr1 = io_lib:format("`~s`", [Name]),
            NR4 = func_rec_str_1('func_index_str', fun() ->
                func_index(Index, NameStr1, EType, SqlType, Comment) end, NR3),
            NR5 = func_rec_str_1('func_var_str', fun() -> func_var(Index, NameStr) end, NR4),
            NR6 = func_create_1(Name, EType, SqlType, Default, Comment, Info, NR5),
            column_str(T, Index + 1, NR6);
        _ ->
            column_str(T, Index + 1, NR3)
    end.

%%%===================================================================
%% @doc
%%  每条记录信息
%% @end
%%%===================================================================
record_str_1(NameStr, TypeStr, Default, Comment, R) ->
    DefaultStr = c_lib:get_bool_value(TypeStr =:= "string", fun() -> "\"" ++ Default ++ "\"" end, fun() ->
        str_lib:term_2_str(Default) end),
    RecStr = "\t" ++ NameStr ++ " = " ++ DefaultStr ++ " :: " ++ TypeStr ++ "(), %%" ++ Comment,
    L = maps:get('rec_str', R, []),
    R#{'rec_str' => [RecStr | L]}.

%%%===================================================================
%% @doc
%%  最后一条记录
%% @end
%%%===================================================================
record_str_2(NameStr, TypeStr, Default, Comment, #{tag_str := TagStr} = R) ->
    DefaultStr = c_lib:get_bool_value(TypeStr =:= "string", fun() -> "\"" ++ Default ++ "\"" end, fun() ->
        str_lib:term_2_str(Default) end),
    RecStr = "\t" ++ NameStr ++ " = " ++ DefaultStr ++ " :: " ++ TypeStr ++ "() %%" ++ Comment,
    L = maps:get('rec_str', R, []),
    Str1 = string:join(lists:reverse([RecStr | L]), "\r\n"),
    Str2 = "-record(" ++ TagStr ++ ", {"
    "\r\n" ++ Str1 ++ "\r\n"
    "})."
    "\r\n\r\n",
    R#{'rec_str' => Str2}.

%%%===================================================================
%% @doc
%%  记录变化
%% @end
%%%===================================================================
func_rec_str_1(Key, F, R) ->
    {FuncL, ExportL} = maps:get(Key, R, {[], []}),
    {FuncStr, ExportStr} = F(),
    R#{Key => {[FuncStr | FuncL], [ExportStr | ExportL]}}.

%%%===================================================================
%% @doc
%%  记录变化
%% @end
%%%===================================================================
func_rec_str_2(Key, F, R) ->
    func_rec_str_2(Key, F, R, true).
func_rec_str_2(Key, F, R, Bool) ->
    {FuncL, ExportL} = maps:get(Key, R, {[], []}),
    {FuncStr, ExportStr} = F(),
    FStr1 = string:join(lists:reverse([FuncStr | FuncL]), "\r\n"),
    FStr2 = FStr1 ++ "\r\n\r\n",
    EStr2 = c_lib:get_bool_value(Bool, fun() ->
        "-export([" ++ string:join(lists:reverse([Var || Var <- [ExportStr | ExportL], Var =/= ""]), ", ") ++ "]).\r\n"
    end, ExportStr),
    R#{Key => {FStr2, EStr2}}.

%%%===================================================================
%% @doc
%% 创建表sql语句
%% @end
%%%===================================================================
func_create_1(Name, EType, SqlType, Default, Comment, Info, R) ->
    StrL = maps:get('func_create_sql', R, []),
    SqlType1 = c_db_util:format_sql_type(SqlType),
    Str = case proplists:get_value('auto_increment', Info) of
        true ->
            io_lib:format("`~s` ~s NOT NULL COMMENT '~ts' ~s", [Name, SqlType1, Comment, "AUTO_INCREMENT"]);
        _ ->
            DefaultStr = c_db_util:format_column_default(EType, SqlType, Default),
            io_lib:format("`~s` ~s~ts COMMENT '~ts'", [Name, SqlType1, DefaultStr, Comment])
    end,
    R#{'func_create_sql' => [Str | StrL]}.

%%%===================================================================
%% @doc
%% 创建表sql语句
%% @end
%%%===================================================================
func_create_2(#{'func_create_sql' := SqlStrL, 'tag_str' := TagStr} = R) ->
    {Str0, Str1, PriKey, EngineStr} = func_create_3(R),
    FStr = "create() ->"
    "\r\n\t<<\"CREATE TABLE  IF NOT EXISTS `" ++ TagStr ++ "` (\r\n"
    "\t\t" ++ string:join(lists:reverse(SqlStrL), ",\r\n\t\t") ++ ",\r\n\t\t" ++ Str0 ++ "\r\n"
    "\t)" ++ " ENGINE=" ++ EngineStr ++ " DEFAULT CHARSET=utf8 " ++ Str1 ++ ";\"/utf8>>.\r\n\r\n",
    EStr = "create/0",
    R#{'func_create_sql' => {FStr, EStr}, 'primary_key' => PriKey}.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
func_create_3(#{'info' := {Column, InfoL}}) ->
    {PriKey, NInfoL0} = case lists:keytake('primary_key', 1, InfoL) of
        {value, {_, Var0}, SubL0} -> {Var0, SubL0};
        false -> {element(1, Column), InfoL}
    end,
    PriKeyStr = ?BOOL_VALUE(is_list(PriKey), begin
        KeyStr0 = lists:flatten(string:join([io_lib:format("`~p`", [V]) || V <- PriKey], ", ")),
        io_lib:format("PRIMARY KEY (~s)", [KeyStr0])
    end, io_lib:format("PRIMARY KEY (`~p`)", [PriKey])),
    {CommentVar, NInfoL1} = case lists:keytake('comment', 1, NInfoL0) of
        false -> {"", InfoL};
        {value, {_, Var1}, SubL1} -> {Var1, SubL1}
    end,
    {AutoIncrStr, NInfoL2} = case lists:keytake('auto_increment', 1, NInfoL1) of
        false -> {"", NInfoL1};
        {value, {_, Var2}, SubL2} ->
            AutoIncrStr0 = lists:flatten(io_lib:format(" AUTO_INCREMENT=~p", [Var2])),
            {AutoIncrStr0, SubL2}
    end,
    ExtraStr = io_lib:format("COMMENT= '~ts'~s", [CommentVar, AutoIncrStr]),
    KeyStrL = [io_lib:format("~s `~s`(`~s`)", [string:uppercase(atom_to_list(Key)), Value, Value]) || {Key, Value} <- NInfoL2, Key =:= 'index' orelse Key =:= 'key'],
    Str = string:join([PriKeyStr | KeyStrL], ",\r\n\t\t"),
    EngineStr = proplists:get_value(engine, InfoL, "Innodb"),
    {Str, ExtraStr, PriKey, EngineStr}.

%%%===================================================================
%% @doc
%%  检测数据函数
%% @end
%%%===================================================================
func_check_str_1(_, _, "term", R) -> R;
func_check_str_1(TagStr, NameStr, TypeStr, R) ->
    L = maps:get('check_func_str', R, []),
    TypeStr1 = c_lib:get_bool_value(TypeStr =:= "string", "list", TypeStr),
    Str = "is_" ++ TypeStr1 ++ "(Rec#" ++ TagStr ++ "." ++ NameStr ++ ")",
    R#{'check_func_str' => [Str | L]}.

%%%===================================================================
%% @doc
%%
%% @end
%%%===================================================================
func_check_str_2(TagStr, NameStr, TypeStr, R) ->
    StrL = maps:get('check_func_str', func_check_str_1(TagStr, NameStr, TypeStr, R), []),
    case StrL of
        [] ->
            R#{'check_func_str' => {"check(_Rec) -> true.\r\n\r\n", "check/1"}};
        _ ->
            Str1 = string:join(StrL, " andalso "),
            R#{'check_func_str' => {"check(Rec) ->\r\n\t" ++ Str1 ++ ".\r\n\r\n", "check/1"}}
    end.

%%%===================================================================
%% @doc
%%  get函数
%% @end
%%%===================================================================
func_get_str(TagStr, NameStr) ->
    FNameStr = "get_" ++ NameStr,
    FuncStr = FNameStr ++ "(#" ++ TagStr ++ "{" ++ NameStr ++ " = Var}) -> Var.",
    ExportStr = FNameStr ++ "/1",
    {FuncStr, ExportStr}.

%%%===================================================================
%% @doc
%%  set函数
%% @end
%%%===================================================================
func_set_str(TagStr, NameStr, TypeStr) ->
    FNameStr = "set_" ++ NameStr,
    FuncStr0 = case TypeStr of
        "term" -> FNameStr ++ "(R, Var)->";
        _ ->
            TypeStr1 = c_lib:get_bool_value(TypeStr =:= "string", "list", TypeStr),
            FNameStr ++ "(R, Var) when is_" ++ TypeStr1 ++ "(Var) ->"
    end,
    FuncStr = FuncStr0 ++ "\r\n\tR#" ++ TagStr ++ "{" ++ NameStr ++ " = Var}.",
    ExportStr = FNameStr ++ "/2",
    {FuncStr, ExportStr}.

%%%===================================================================
%% @doc
%%  通过index找信息,空处理及导出涵数
%% @end
%%%===================================================================
func_last_index() ->
    FuncStr = "info_by_index(_) -> none.",
    {FuncStr, "info_by_index/1"}.

%%%===================================================================
%% @doc
%%  通过index找信息
%% @end
%%%===================================================================
func_index(Index, Var, EType, SType, Comment) ->
    FNameStr = "info_by_index",
    Str = io_lib:format("{<<\"~s\">>, '~s', ~s, <<\"~ts\">>}", [Var, EType, str_lib:term_2_str(SType), unicode:characters_to_binary(Comment)]),
    FuncStr = FNameStr ++ "(" ++ integer_to_list(Index) ++ ") ->\r\n\t" ++ Str ++ ";",
    {FuncStr, ""}.

%%%===================================================================
%% @doc
%%  通过字段找信息,空字段处理及导出声明
%% @end
%%%===================================================================
func_last_var() ->
    FuncStr = "index_by_var(_) -> none.",
    {FuncStr, "index_by_var/1"}.

%%%===================================================================
%% @doc
%%  通过字段找信息
%% @end
%%%===================================================================
func_var(Index, Var) ->
    FNameStr = "index_by_var",
    FuncStr = FNameStr ++ "(" ++ str_lib:term_2_str(list_to_binary(Var)) ++ ") -> " ++ str_lib:term_2_str(Index) ++ ";",
    {FuncStr, ""}.

%%%===================================================================
%% @doc
%% select函数
%% @end
%%%===================================================================
func_select(TagStr, #{'primary_key' := [PriKey | _]}) when is_atom(PriKey) ->
    func_select(TagStr, #{'primary_key' => PriKey});
func_select(TagStr, #{'primary_key' := PriKey}) when is_atom(PriKey) ->
    Str = "select(DB) ->\r\n\t"
    "list_to_binary(io_lib:format(<<\"SELECT * FROM ~s.`" ++ TagStr ++ "` WHERE `" ++ atom_to_list(PriKey) ++ "` = ?;\">>, [DB])).\r\n\r\n",
    {Str, "select/1"}.

%%%===================================================================
%% @doc
%% 删除函数
%% @end
%%%===================================================================
func_delete(TagStr, #{'primary_key' := [PriKey | _]}) when is_atom(PriKey) ->
    func_delete(TagStr, #{'primary_key' => PriKey});
func_delete(TagStr, #{'primary_key' := PriKey}) when is_atom(PriKey) ->
    Str = "delete(DB) ->\r\n\t"
    "list_to_binary(io_lib:format(<<\"DELETE FROM ~s.`" ++ TagStr ++ "` WHERE `" ++ atom_to_list(PriKey) ++ "` = ?;\">>, [DB])).\r\n\r\n",
    {Str, "delete/1"}.


%%%===================================================================
%% @doc
%%  获取主键
%% @end
%%%===================================================================
primary_key_str(#{'primary_key' := PriKey}) when is_list(PriKey) ->
    KeyL = list_to_tuple([<<$`, (atom_to_binary(V))/binary, $`>> || V <- PriKey]),
    KeyStr = str_lib:term_2_str(KeyL),
    Str = "primary_key() -> \r\n\t" ++ KeyStr ++ ".\r\n\r\n",
    {Str, "primary_key/0"};
primary_key_str(#{'primary_key' := PriKey}) when is_atom(PriKey) ->
    Str = "primary_key() -> \r\n\t<<\"`" ++ atom_to_list(PriKey) ++ "`\">>.\r\n\r\n",
    {Str, "primary_key/0"}.

%%%===================================================================
%% @doc
%%  表名
%% @end
%%%===================================================================
sql_table_str(TagStr) ->
    Str = "table_name() ->\r\n\t<<\"`" ++ TagStr ++ "`\">>.\r\n\r\n",
    {Str, "table_name/0"}.

%%%===================================================================
%% @doc
%% 储存类型
%% @end
%%%===================================================================
save_type_func(Type) ->
    Str = "save_type() ->\r\n\t" ++ str_lib:term_2_str(Type) ++ ".\r\n\r\n",
    {Str, "-export([save_type/0]).\r\n"}.

%%%===================================================================
%% @doc
%%  合并导出函数
%% @end
%%%===================================================================
export_func(L) ->
    Str = string:join(L, ", "),
    "-export([" ++ Str ++ "]).\r\n".

%%%===================================================================
%% @doc
%%  sql mod文件头部
%% @end
%%%===================================================================
total_head_str(FileTag) ->
    Mod = case FileTag of
        ?C_DB_FILE_TAG -> dbT;
        ?C_BI_FILE_TAG -> biT
    end,
    Str =
        "%%%==================================================================="
        "\r\n%% @doc"
        "\r\n%% ~s \r\n%% @end"
        "\r\n%%%==================================================================="
        "\r\n-module(~s)\.\r\n"
        "-author(sql)\.\r\n"
        "\r\n",
    io_lib:format(Str, [Mod, Mod]).

%%%===================================================================
%% @doc
%% 模块列表函数
%% @end
%%%===================================================================
mod_func(L) ->
    F = fun({Mod, Tag, _, _}, R) -> o_tuple:set(Tag, {Tag, Mod}, R) end,
    DataT = c_lib:foreach(F, {}, L),
    Str = str_lib:term_2_str1(DataT),
    FuncStr = "get_mod() ->"
    "\r\n\t" ++ Str ++ ".\r\n\r\n",
    {FuncStr, "get_mod/0"}.

%%%===================================================================
%% @doc
%% 获取md5
%% @end
%%%===================================================================
md5_func(L) ->
    F = fun({_, Tag, Md5, _}) -> "get_md5(" ++ str_lib:term_2_str(Tag) ++ ") ->\r\n\t" ++ str_lib:term_2_str(Md5) end,
    StrL = [F(Var) || Var <- L, element(3, Var) =/= none],
    EndF = "get_md5(_) -> \r\n\t none",
    Str0 = string:join(lists:reverse([EndF | StrL]), ";\r\n"),
    Str = Str0 ++ ".\r\n\r\n",
    {Str, "get_md5/1"}.

%%%===================================================================
%% @doc
%% 获取md5
%% @end
%%%===================================================================
file_func(L) ->
    F = fun({_, Tag, _, File}) ->
        "get_file(" ++ str_lib:term_2_str(Tag) ++ ") ->\r\n\t" ++ str_lib:term_2_str(File) end,
    StrL = [F(Var) || Var <- L, element(4, Var) =/= 'none'],
    EndF = "get_file(_) ->\r\n\tnone",
    Str0 = string:join(lists:reverse([EndF | StrL]), ";\r\n"),
    Str = Str0 ++ ".\r\n\r\n",
    {Str, "get_file/1"}.

%%%===================================================================
%% @doc
%% 键index
%% @end
%%%===================================================================
key_index(ColumnL, InfoL) ->
    RecIndex1st = 2, %%记录有效值从第二个开始
    case proplists:get_value('primary_key', InfoL, none) of
        none -> {RecIndex1st, element(1, hd(ColumnL))};
        [_, _ | _] = ColumnKey -> keyIndex_l(RecIndex1st, ColumnL, ColumnKey);
        [ColumnKey] -> keyIndex_1(RecIndex1st, ColumnL, ColumnKey);
        ColumnKey -> keyIndex_1(RecIndex1st, ColumnL, ColumnKey)
    end.

%%%===================================================================
%% @doc
%% 多键key
%% @end
%%%===================================================================
keyIndex_l(RecIndex1st, ColumnL, ColumnKey) ->
    F = fun(Key, Acc) ->
        FF = fun(Column0, Index0) ->
            case element(1, Column0) of
                Key -> {break, {ok, Index0, Column0}};
                _ -> Index0 + 1
            end
        end,
        {ok, Index, Column} = c_lib:foreach(FF, RecIndex1st, ColumnL),
        [{Index, Column} | Acc]
    end,
    KVL = lists:reverse(c_lib:foreach(F, [], ColumnKey)),
    {IndexL, KeyColumnL} = lists:unzip(KVL),
    {"key_index() ->\r\n\t" ++ str_lib:term_2_str(list_to_tuple(IndexL)) ++ ".\r\n\r\n", "key_index/0", KeyColumnL}.

%%%===================================================================
%% @doc
%% 单主键
%% @end
%%%===================================================================
keyIndex_1(RecIndex1st, ColumnL, ColumnKey) ->
    F = fun(Column, Index) ->
        case element(1, Column) of
            ColumnKey -> {break, {ok, Index, Column}};
            _ -> Index + 1
        end
    end,
    {ok, Index, Column} = c_lib:foreach(F, RecIndex1st, ColumnL),
    {"key_index() ->\r\n\t" ++ integer_to_list(Index) ++ ".\r\n\r\n", "key_index/0", Column}.

%%%%===================================================================
%% @doc
%% 检查类型
%% @end
%%%===================================================================
check(integer, Type2) ->
    {ok, MC} = re:compile("^(?:int|bigint)(?:\\(\\d+\\)|)$", [caseless]),
    case re:run(atom_to_list(Type2), MC, [global, {capture, all, list}]) of
        {match, _} -> ok;
        Error -> error({Error, integer, Type2})
    end;
check(float, Type2) ->
    {ok, MC} = re:compile("^(?:float|double)(?:\\(\\d+\\)|\\(\\d+,\\d+\\)|)$", [caseless]),
    case re:run(atom_to_list(Type2), MC, [global, {capture, all, list}]) of
        {match, _} -> ok;
        Error -> error({Error, float, Type2})
    end;
check(_, _) ->
    ok.
