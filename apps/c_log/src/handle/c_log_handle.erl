%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2021, ckc personal
%%% @doc
%%%
%%% @end
%%% Created : 01. 8月 2021 17:03
%%%-------------------------------------------------------------------
-module(c_log_handle).
-author("chenkecai").

-behaviour(gen_server).

%%%% API
%%-export([start_link/0]).

%% gen_server callbacks
-export([start_link/2]).
-export([adding_handler/1, removing_handler/1, log/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMER_LOOP_DOING_SEC, 60000).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% 启动
%%%===================================================================
start_link(Id, Config) ->
    SubConfig = maps:get(config, Config),
    {ok, Pid} = gen_server:start_link({local, proc_name(Id)}, ?MODULE, SubConfig#{id => Id}, []),
    logger:remove_handler(Id),
    ok = logger:add_handler(Id, ?MODULE, Config#{config => SubConfig#{pid => Pid}}),
    {ok, Pid}.

%%%===================================================================
%% @doc
%%  进程序名字
%% @end
%%%===================================================================
proc_name(Id) ->
    list_to_atom(io_lib:format("log_~s", [Id])).

%%%===================================================================
%%% 添加回调
%%%===================================================================
adding_handler(#{'level' := Level, 'filter_compare' := CompareOpt} = Config) ->
    NCfg0 = Config#{filters => [
        {filter1st, {fun logger_filters:level/2, {log, CompareOpt, Level}}}
    ]},
    NCfg = maps:remove('filter_compare', NCfg0),
    {ok, NCfg};
adding_handler(Config) ->
    {ok, Config}.


%%%===================================================================
%%%
%%%===================================================================
removing_handler(#{config := #{pid := Pid}}) ->
    gen_server:cast(Pid, {stop, normal}).

%%%===================================================================
%%%
%%%===================================================================
log(LogEvent, #{config := #{pid := Pid}} = Config) ->
    gen_server:cast(Pid, {log, LogEvent, Config}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: map()} | {ok, State :: map(), timeout() | hibernate} |
    {stop, Reason :: map()} | ignore).
init(#{id := Id} = Cfg) ->
    FileInfo = c_log_file:read_files_name(Id),
    #{'file_loop_time' := LoopTime, 'max_heap_size' := MaxHeapSize} = NCfg = init_cfg(Cfg),
    File = case c_log_file:try_create_file(Id, FileInfo, LoopTime) of
        {ok, FileVar, NFileInfo} ->
            put('file_info', NFileInfo), FileVar;
        {ok, FileVar} ->
            put('file_info', FileInfo), FileVar
    end,
    erlang:process_flag('max_heap_size', MaxHeapSize),
    {ok, Fd} = c_log_file:open_file(File),
    erlang:start_timer(?TIMER_LOOP_DOING_SEC, self(), 'timer_loop_handle'),
    {ok, NCfg#{file => File, fd => Fd}}.

init_cfg(Cfg) ->
    LoopTime = maps:get('file_loop_time', Cfg, 43200),
    MaxHeapSize = maps:get('max_heap_size', Cfg, 16#fffff),
    SaveFileTime = maps:get('save_file_time', Cfg, 604800),
    Cfg#{'file_loop_time' => max(3600, LoopTime), 'max_heap_size' => MaxHeapSize, save_file_time => max(86400, SaveFileTime)}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: map()) ->
    {reply, Reply :: term(), NewState :: map()} |
    {reply, Reply :: term(), NewState :: map(), timeout() | hibernate} |
    {noreply, NewState :: map()} |
    {noreply, NewState :: map(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: map()} |
    {stop, Reason :: term(), NewState :: map()}).
handle_call('clear_log', _From, #{file := File, fd := Fd} = State) ->
    case file:close(Fd) of
        ok ->
            Result = file:write_file(File, <<>>),
            {ok, NewFd} = c_log_file:open_file(File),
            {reply, Result, State#{fd => NewFd}};
        CloseR ->
            {reply, {'close_fail', CloseR}, State}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: map()) ->
    {noreply, NewState :: map()} |
    {noreply, NewState :: map(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: map()}).
handle_cast({log, LogEvent, Config}, #{fd := Fd} = State) ->
    do_log(Fd, LogEvent, Config),
    {noreply, State};
handle_cast({stop, normal}, State) ->
    {stop, normal, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: map()) ->
    {noreply, NewState :: map()} |
    {noreply, NewState :: map(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: map()}).
handle_info({timeout, _TimerRef, 'timer_loop_handle'}, Cfg) ->
    FileInfo = get('file_info'),
    erlang:start_timer(?TIMER_LOOP_DOING_SEC, self(), 'timer_loop_handle'),
    case c_log_file:time_doing(FileInfo, Cfg) of
        {ok, NCfg} -> {noreply, NCfg};
        {ok, NFileInfo, NCfg} ->
            put('file_info', NFileInfo),
            {noreply, NCfg}
    end;
handle_info(_Info, State) ->
    {reply, {error, bad_request}, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: map()) -> term()).
terminate(_Reason, #{fd := Fd, id := Id}) ->
    logger:remove_handler(Id),
    _ = file:close(Fd),
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: map(), Extra :: term()) ->
    {ok, NewState :: map()} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%%
%%%===================================================================
do_log(Fd, LogEvent, #{formatter := {FModule, FConfig}}) ->
    String = FModule:format(LogEvent, FConfig),
    io:put_chars(Fd, String).