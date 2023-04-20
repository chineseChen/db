%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 03. 8月 2021 15:17
%%%-------------------------------------------------------------------
-module(c_log_sup).
-author("chenkecai").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
init([]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},
    file:make_dir("log"),
    {ok, ChildL} = add_handle(),
    case logger:get_handler_config('simple') of
        {ok, _} -> logger:remove_handler('simple');
        _ -> ok
    end,
    {ok, {SupFlags, ChildL}}.

add_handle() ->
    {ok, ArgL} = application:get_env('handler'),
    F = fun({Tag, Handle, Cfg}) ->
        #{id => Tag,
            start => {Handle, start_link, [Tag, Cfg]},
            restart => transient,
            shutdown => 2000,
            type => worker,
            modules => []}
    end,
    {ok, [F(Arg) || Arg <- ArgL]}.