-module(rebar3_db2file).

-export([init/1, load_code/0]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_db2file_compile:init(State),
    {ok, State2} = rebar3_db2file_clean:init(State1),
    {ok, State2}.

%%%===================================================================
%% @doc
%% 加载必须的代码
%% @end
%%%===================================================================
load_code() ->
    {ok, WorkDir} = file:get_cwd(),
    CPath = filename:join(WorkDir, "_build/default/lib/c_lib/ebin"),
    BeamFile = filename:join(CPath, "db_2_file.beam"),
    case filelib:is_file(BeamFile) of
        true ->
            code:add_path(CPath);
        false ->
            PathReq = filename:join(WorkDir, "apps/c_lib/src/*/*.erl"),
            CodeL = filelib:wildcard(PathReq),
            F = fun(File) ->
                {ok, Mod, Binary} = compile:file(File, [binary]),
                code:load_binary(Mod, File, Binary),
                ok
            end,
            lists:foreach(F, CodeL)
    end,
    code_lib:compile_load([
        "apps/c_db/autoSrc/dbT/dbT.erl",
        "apps/c_bi/autoSrc/biT/biT.erl"
    ]),
    ok.

