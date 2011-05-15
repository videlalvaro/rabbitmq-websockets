{application, rabbitmq_websockets,
 [{description, "Embedded Rabbit Metronome"},
  {vsn, "0.01"},
  {modules, [
    rabbit_websockets,
    rabbit_websockets_consumer,
    rabbit_websockets_sup,
    rabbit_websockets_util,
    rabbit_websockets_worker
  ]},
  {registered, []},
  {mod, {rabbit_websockets, []}},
  {env, [{misultin_port, 8080}]},
  {applications, [kernel, stdlib, rabbit, amqp_client]}]}.
