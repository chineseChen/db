%%%-------------------------------------------------------------------
%%% @author chenkecai
%%% @copyright (C) 2022, joinGame
%%% @doc
%%% 改自mysql v1.8.0,query实现异步
%%% 将gen_server:call,改为gen_server:send_request
%%% handle_info 接收
%%% @end
%%% Created : 08. 10月 2022 9:42
%%%-------------------------------------------------------------------
-module(c_mysql).
-author("chenkecai").

%% API
-export([query/2, query/3, query/4, query/5]).
-export([query_call/2, requestBackInfo/1, call_back/1]).


%% @doc Executes a plain query.
%% @see query/5.
%%-spec query(Conn, Query) -> Result
%%    when Conn :: connection(),
%%    Query :: iodata(),
%%    Result :: query_result().
query(Conn, Query) ->
    query_helper(Conn, Query, no_params, no_filtermap_fun, default_timeout).

%% @doc Executes a query.
%% @see query/5.
%%-spec query(Conn, Query, Params | FilterMap | Timeout) -> Result
%%    when Conn :: connection(),
%%    Query :: query(),
%%    Timeout :: timeout(),
%%    Params :: [query_param()],
%%    FilterMap :: query_filtermap_fun(),
%%    Result :: query_result().
query(Conn, Query, Params) when Params == no_params;
    is_list(Params) ->
    query_helper(Conn, Query, Params, no_filtermap_fun, default_timeout);
query(Conn, Query, FilterMap) when FilterMap == no_filtermap_fun;
    is_function(FilterMap, 1);
    is_function(FilterMap, 2) ->
    query_helper(Conn, Query, no_params, FilterMap, default_timeout);
query(Conn, Query, Timeout) when Timeout == default_timeout;
    is_integer(Timeout);
    Timeout == infinity ->
    query_helper(Conn, Query, no_params, no_filtermap_fun, Timeout).

%% @doc Executes a query.
%% @see query/5.
%%-spec query(Conn, Query, Params, Timeout) -> Result
%%    when Conn :: connection(),
%%    Query :: query(),
%%    Timeout :: timeout(),
%%    Params :: [query_param()],
%%    Result :: query_result();
%%    (Conn, Query, FilterMap, Timeout) -> Result
%%    when Conn :: connection(),
%%    Query :: query(),
%%    Timeout :: timeout(),
%%    FilterMap :: query_filtermap_fun(),
%%    Result :: query_result();
%%    (Conn, Query, Params, FilterMap) -> Result
%%    when Conn :: connection(),
%%    Query :: query(),
%%    Params :: [query_param()],
%%    FilterMap :: query_filtermap_fun(),
%%    Result :: query_result().
query(Conn, Query, Params, Timeout) when (Params == no_params orelse
    is_list(Params)) andalso
    (Timeout == default_timeout orelse
        is_integer(Timeout) orelse
        Timeout == infinity) ->
    query_helper(Conn, Query, Params, no_filtermap_fun, Timeout);
query(Conn, Query, FilterMap, Timeout) when (FilterMap == no_filtermap_fun orelse
    is_function(FilterMap, 1) orelse
    is_function(FilterMap, 2)) andalso
    (Timeout == default_timeout orelse
        is_integer(Timeout) orelse
        Timeout =:= infinity) ->
    query_helper(Conn, Query, no_params, FilterMap, Timeout);
query(Conn, Query, Params, FilterMap) when (Params == no_params orelse
    is_list(Params)) andalso
    (FilterMap == no_filtermap_fun orelse
        is_function(FilterMap, 1) orelse
        is_function(FilterMap, 2)) ->
    query_helper(Conn, Query, Params, FilterMap, default_timeout).


%%-spec query(Conn, Query, Params, FilterMap, Timeout) -> Result
%%    when Conn :: connection(),
%%    Query :: query(),
%%    Timeout :: timeout(),
%%    Params :: [query_param()],
%%    FilterMap :: query_filtermap_fun(),
%%    Result :: query_result().
query(Conn, Query, Params, FilterMap, Timeout) ->
    query_helper(Conn, Query, Params, FilterMap, Timeout).

%%-spec query_helper(Conn, Query, Params, FilterMap, Timeout) -> Result
%%    when Conn :: connection(),
%%    Query :: query(),
%%    Timeout :: default_timeout | timeout(),
%%    Params :: no_params | [query_param()],
%%    FilterMap :: no_filtermap_fun | query_filtermap_fun(),
%%    Result :: query_result().
query_helper(Conn, Query, no_params, FilterMap, Timeout) ->
    query_call(Conn, {query, Query, FilterMap, Timeout});
query_helper(Conn, Query, Params, FilterMap, Timeout) ->
    case mysql_protocol:valid_params(Params) of
        true ->
            query_call(Conn,
                {param_query, Query, Params, FilterMap, Timeout});
        false ->
            error(badarg)
    end.

%% --- Helpers ---

%% @doc Makes a gen_server call for a query (plain, parametrized or prepared),
%% checks the reply and sometimes throws an exception when we need to jump out
%% of a transaction.
query_call(Conn, CallReq) ->
    gen_server:send_request(Conn, CallReq).

%%%%===================================================================
%% @doc
%% send_request行为后续处理
%% gen_server info msg处理
%% @end
%%%===================================================================
requestBackInfo({{'$gen_request_id', MRef}, Result}) ->
    erlang:demonitor(MRef, [flush]),
    Result.

%%%===================================================================
%% @doc
%% mysql 查询结果分析
%% @end
%%%===================================================================
call_back({implicit_commit, _NestingLevel, Query}) ->
    logger:error("mode:~p line:~p reason:~p", [?MODULE, ?LINE, {implicit_commit, Query}]),
    error;
call_back({implicit_rollback, _NestingLevel, _ServerReason} = ImplicitRollback) ->
    logger:error("mode:~p line:~p reason:~p", [?MODULE, ?LINE, ImplicitRollback]),
    error;
call_back(Result) ->
    Result.
