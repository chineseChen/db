%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(db_cache_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
%%-export([start_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    %%不用simple_one_for_one 因为要用tag
    {ok, {#{strategy => one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts}, child_spec_l()}}.

%%%===================================================================
%% @doc
%% child启动参数
%% @end
%%%===================================================================
child_spec_l() ->
    case erlang:function_exported(dbT, get_mod, 0) of
        true ->
            ModInfoT = dbT:get_mod(),
            F = fun
                (Acc, _, {Tag, Mod}) when is_atom(Mod) ->
                    {ok, AppDbSrcL} = c_db_app:get_db_src(),
                    ModDBSrcL = Mod:db_src(),
                    [child_spec({{DBSrc, Tag}, Mod}) || DBSrc <- ModDBSrcL, lists:member(DBSrc, AppDbSrcL), DBSrc =/= 'sys'] ++ Acc;
                (Acc, _, {Tag, ModDBSrcL}) ->
                    {ok, AppDbSrcL} = c_db_app:get_db_src(),
                    [child_spec({{DBSrc, Tag}, none}) || DBSrc <- ModDBSrcL, lists:member(DBSrc, AppDbSrcL), DBSrc =/= 'sys'] ++ Acc
            end,
            c_lib:tuple_foreach(F, [], ModInfoT);
        false -> []
    end.

%%%%%===================================================================
%%%% @doc
%%%% child启动参数
%%%% @end
%%%%%===================================================================
child_spec({STag, _} = ModInfo) ->
    #{id => STag,
        start => {'db_cache_proc', start_link, [ModInfo]},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => ['db_cache_proc']
    }.
