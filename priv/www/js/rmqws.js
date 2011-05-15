var RabbitMQWs = function() {

    var that = {
        ws: null,

        start: function() {
            if ("WebSocket" in window) {
              // browser supports websockets
              this.ws = new WebSocket("ws://" + window.location.host + "/service");
              var that = this;
              this.ws.onopen = function() {
                  // websocket is connected
                  $(that).trigger('rmqws-onconnection-status', ['connected']);
              };
              this.ws.onmessage = function (evt) {
                  // that.displayMessage("#messages", evt.data);
                  $(that).trigger('rmqws-onmessage', [evt.data]);
              };
              this.ws.onclose = function() {
                  // websocket was closed
                  $(that).trigger('rmqws-onconnection-status', ['disconnected']);
              };

            } else {
                // browser does not support websockets
                $(that).trigger('rmqws-onerror', ["Sorry, your browser does not support websockets."]);
            }
        },

        switchExchange: function(exchange, routing_key) {
            var exchange = exchange;
            var routing_key = routing_key;
            if(exchange.length > 0) {
                this.ws.send(exchange + ':' + routing_key);
            }
        }
    };

    window.onunload = function() {
        if(that.ws) {
            that.ws.close();
        }
    };

    return that;
};