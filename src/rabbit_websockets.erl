-module(rabbit_websockets).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    rabbit_websockets_sup:start_link().

stop(_State) ->
    ok.