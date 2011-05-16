# RabbitMQ Websockets Plugin #

This plugin exposes Websockets for RabbitMQ.

The user connects to the host where RabbitMQ is running, using default port `8080`.

The user can use a form to submit to which `exchange` wants to receive messages and can provide an optional `routing_key`.

The plugin will attach a consumer to the provided exchange and will start sending messages to the connected websocket processes.

Depending on the `type` of the exchange will be the behavior seen browser side.

**Warning:** As with every RabbitMQ plugin, if this plugin crashes the broker may crash too. Be warned!.

This plugin doesn't implement any client throttling. So if users can connect to your Websockets endpoint they can start getting messages from your server.

Maybe in the future I may add authentication options for the AMQP connections. If you would like to use this plugin and have a need for such features please open a ticket on Github. Also you could fork the project and implement such features ;-).

## Installation ##

Get the `rabbitmq-public-umbrella`

    $ hg clone http://hg.rabbitmq.com/rabbitmq-public-umbrella
    $ cd rabbitmq-public-umbrella
    $ make co

Get the [misultin_wrapper](https://github.com/videlalvaro/misultin_wrapper) to support Websockets:

Inside the `rabbitmq-public-umbrella` directory do:

    $ git clone git://github.com/videlalvaro/misultin_wrapper.git

Then clone this repository:

    $ git clone git://github.com/videlalvaro/rabbitmq-websockets.git

Once you have the code you can move into the `rabbitmq-websockets` directory and test the plugin with the broker:

    $ make run-in-broker

If you want to install the plugin in the broker then copy the `*.ez` files inside the `dist` folder to your broker `plugins` folder. Don't copy the `rabbit_common-*.ez` file.

## Usage ##

Point your browser to [http://localhost:8080](http://localhost:8080) and then submit the name of an __existing exchange__. The routing key is optional and depends on how you publish messages to that exchange.

You can publish test messages by calling the following helper function inside the Erlang REPL:

    rabbit_websockets_util:publish_msg(Exchange, Msg, RKey).

All three parameters are binaries, for example:

    rabbit_websockets_util:publish_msg(<<"amq.fanout">>, <<"Hello Websockets!">>, <<"">>).

## Configuration ##

This plugin has two parameters that affect its behavior:

`misultin_port`: The port Misultin should listen to. Default value is `8080`.

`message_handler`: an Erlang tuple with two atoms like {module, function}. Default value is `{rabbit_websockets_util, basic_handler}`. The message handler will be used to decode the message. It should return the message in such a way that is suitable to send over websockets. For example, if you are sending JSON messages and you are only interested in one field of the JSON object, then you can provide a handler that will decode the JSON object, extract the field, and return that value.

You can modify such settings on your `rabbitmq.config` file like this:

    [
      {rabbit, [
        ...
        %% list of RabbitMQ options
        ]},
      {rabbitmq_websockets, [
              {misultin_port, 8081},
              {message_handler, {my_module, my_function}} ]}
    ].

## Modifying the UI ##

To implement your own UI you need to have the following javascript files in your HTML:

    <script type="text/javascript" src="/js/jquery.min.js"></script>
    <script type="text/javascript" src="/js/rmqws.js"></script>

A sample view can be found on `priv/www/index.html`

The `RabbitMQWs` object will fire the following events:

`rmqws-connection-status`: passes the connection status as parameter to the event handler.

`rmqws-onmessage`: passes the message received from RabbitMQW to the event handler.

`rmqws-onerror`: signals an error to the event handler.

A basic Javascript view can be made like this:

    <!DOCTYPE html>
    <html>
      <head>
        <title>RabbitMQ Websockets Plugin - Basic Test</title>
        <link rel="icon" href="/favicon.ico">
      </head>
      <body>
        <h1>RabbitMQ Websockets Plugin - Basic Test</h1>
        <ul id="messages"></ul>
        <script type="text/javascript" src="/js/jquery.min.js"></script>
        <script type="text/javascript" src="/js/rmqws.js"></script>
        <script type="text/javascript">
            (function($) {

                function displayMessage(message) {
                    $("#messages").append("<li>"+message+"</li>").attr({ scrollTop: $("#messages").attr("scrollHeight") });
                };

                var RMQ_WS_PORT = 8080;

                $(document).ready(function() {
                  var rmqws = new RabbitMQWs();

                  rmqws.start(window.location.hostname + ':' + RMQ_WS_PORT);

                  $(rmqws).bind('rmqws-onconnection-status', function(event, status){
                      if(status == 'connected') {
                          rmqws.switchExchange('amq.fanout', "");
                      }
                      displayMessage(status);
                  });

                  $(rmqws).bind('rmqws-onmessage', function(event, msg){
                      displayMessage(msg);
                  });

                  $(rmqws).bind('rmqws-onerror', function(event, error){
                      displayMessage(error);
                  });
              });
            })(jQuery);
        </script>
      </body>
    </html>

Copy the contents of the example to your `index.html` file and server that from your webserver. Modify `RMQ_WS_PORT` according to your settings.

I created a sample project showing how to use this plugin: [rmq_ws_test](git clone git://github.com/videlalvaro/rmq_ws_test.git)

## License ##

See LICENSE.md