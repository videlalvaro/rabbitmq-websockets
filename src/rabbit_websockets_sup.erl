-module(rabbit_websockets_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->
    {ok, Port} = application:get_env(misultin_port),
    {ok, {{one_for_one, 3, 10},
          [{rabbit_websockets_worker,
            {rabbit_websockets_worker, start_link, [Port]},
            permanent,
            10000,
            worker,
            [rabbit_websockets_worker]}
          ]}}.