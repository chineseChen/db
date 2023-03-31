%%%-------------------------------------------------------------------
%% @doc c_bi public API
%% @end
%%%-------------------------------------------------------------------

-module(c_bi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    c_bi_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
