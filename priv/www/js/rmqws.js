(function($) {
    var RabbitMQWs = function() {
        this.ws = null;
        this.exchange = null;
        var that = this;
        window.onunload = function() {
            if(that.ws) {
                that.ws.close();
            }
        }
    };

    RabbitMQWs.prototype.connect = function() {
        if ("WebSocket" in window) {
          // browser supports websockets
          this.ws = new WebSocket("ws://" + window.location.host + "/service");
          var that = this;
          this.ws.onopen = function() {
              // websocket is connected
              that.toggleConnStatus('connected');
          };
          this.ws.onmessage = function (evt) {
              that.displayMessage("#messages", evt.data);
          };
          this.ws.onclose = function() {
              // websocket was closed
              that.toggleConnStatus('disconnected');
              that.displayError("You got disconnected from the server");
          };

        } else {
            // browser does not support websockets
            this.displayError("Sorry, your browser does not support websockets.");
        }
    };

    RabbitMQWs.prototype.displayError = function(msg) {
        alert(msg);
    };

    RabbitMQWs.prototype.toggleConnStatus = function(status) {
        this.displayMessage("#info", status);
    };

    RabbitMQWs.prototype.displayMessage = function(where, message) {
        $(where).append("<li>"+message+"</li>").attr({ scrollTop: $(where).attr("scrollHeight") });
    };

    RabbitMQWs.prototype.switchExchange = function(exchange, routing_key) {
        var exchange = $.trim(exchange);
        var routing_key = $.trim(routing_key);
        if(exchange.length > 0) {
            this.ws.send(exchange + ':' + routing_key);
            this.displayMessage("#info", "Connected to: " + exchange + " using: " + routing_key);
            this.exchange = exchange;
        }
        $('form#exchange').get(0).reset();
    };

    RabbitMQWs.prototype.start = function(name, email) {
        this.connect();
    };

    $(document).ready(function() {
        var rmqws = new RabbitMQWs();
        rmqws.start();
        $('form#exchange').submit(function(e) {
            e.preventDefault();
            rmqws.switchExchange($('#name').val(), $('#routing_key').val());
            return false;
        });
    });
})(jQuery);