-module(rabbit_websockets_consumer).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export([start/1]).
-export([stop/1]).

-record(state, {channel, reply_pid, consumer_tag}).

start(Opts) ->
    gen_server:start(?MODULE, Opts, []).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Channel, Exchange, RKey, ReplyPid]) ->
    #'queue.declare_ok'{queue = ConsumerQ}
        = amqp_channel:call(Channel, #'queue.declare'{exclusive = true, auto_delete = true}),

    #'queue.bind_ok'{} = amqp_channel:call(Channel,
                            #'queue.bind'{queue = ConsumerQ, exchange = Exchange, routing_key = RKey}),

    #'basic.consume_ok'{consumer_tag = CTag} =
        amqp_channel:subscribe(Channel, #'basic.consume'{queue = ConsumerQ, no_ack = true}, self()),

    {ok, #state{channel = Channel, reply_pid = ReplyPid, consumer_tag = CTag}}.

handle_info(shutdown, State) ->
    {stop, normal, State};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
    {stop, normal, State};

handle_info({#'basic.deliver'{consumer_tag = CTag},
             #amqp_msg{payload = Msg}},
             #state{reply_pid = ReplyPid, consumer_tag = CTag} = State) ->

    ReplyPid ! {amqp_msgs, Msg},

    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = Channel}) ->
    amqp_channel:close(Channel),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.