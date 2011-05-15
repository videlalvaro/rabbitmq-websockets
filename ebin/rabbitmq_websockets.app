{application, rabbitmq_websockets,
 [{description, "AMQP to Websockets bridge"},
  {vsn, "%%VSN%%"},
  {modules, [
    rabbit_websockets,
    rabbit_websockets_consumer,
    rabbit_websockets_sup,
    rabbit_websockets_util,
    rabbit_websockets_worker
  ]},
  {registered, []},
  {mod, {rabbit_websockets, []}},
  {env, [ {misultin_port, 8080},
          {message_handler, {rabbit_websockets_util, basic_handler}} ]},
  {applications, [kernel, stdlib, rabbit, amqp_client]}]}.
