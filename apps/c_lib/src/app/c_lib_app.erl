%%%-------------------------------------------------------------------
%% @doc c_lib public API
%% @end
%%%-------------------------------------------------------------------

-module(c_lib_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    c_lib_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
