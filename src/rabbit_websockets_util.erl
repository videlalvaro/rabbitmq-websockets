-module(rabbit_websockets_util).

-export([publish_msg/3, basic_handler/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

publish_msg(Exchange, Msg, RKey) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Publish = #'basic.publish'{exchange = Exchange, routing_key = RKey},
    amqp_channel:call(Channel, Publish, #amqp_msg{payload = term_to_binary(Msg)}),
    ok.

basic_handler(Msg) ->
    binary_to_term(Msg).