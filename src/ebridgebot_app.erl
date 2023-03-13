%%%-------------------------------------------------------------------
%% @doc ebridgebot public API
%% @end
%%%-------------------------------------------------------------------

-module(ebridgebot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ebridgebot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
