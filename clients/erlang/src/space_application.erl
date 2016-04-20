-module(space_application).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    crypto:start(),
    ssl:start(),
    space:start_link().

stop(_State) ->
    ok.
