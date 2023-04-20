%%%-------------------------------------------------------------------
%% @doc c_log public API
%% @end
%%%-------------------------------------------------------------------

-module(c_log_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    c_log_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
