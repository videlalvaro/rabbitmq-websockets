-module(rabbit_websockets_worker).
-behaviour(gen_server).

-export([start_link/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {connection, port}).
-record(http_state, {req}).
-record(websocket_state, {ws, conn, consumer, handler}).

start_link(Port) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Port], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%---------------------------
% Gen Server Implementation
% --------------------------

init([Port]) ->
    process_flag(trap_exit, true),
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Fun} = application:get_env(message_handler),

    misultin:start_link([{port, Port},
                        {loop, fun(Req) -> handle_http(#http_state{req=Req}) end},
                        {ws_loop,
                         fun(Ws) ->
                                 handle_websocket(#websocket_state{ws=Ws, conn=Connection, handler=Fun})
                         end},
                         {ws_autoexit, false}]),
    erlang:monitor(process, misultin),

    {ok, #state{connection = Connection, port = Port}}.

handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply,State}.

handle_info({'DOWN', _, _, {misultin, _}, _}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, #state{connection = Connection}) ->
    amqp_connection:close(Connection),
    misultin:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-----------------------------
% HTTP Handling Implementation
% ----------------------------

handle_http(#http_state{req=Req}=State) ->
    handle(Req:get(method), Req:resource([lowercase, urldecode]), State).

handle('HEAD', [], #http_state{req=Req}) ->
    Req:ok("");

handle('GET', [], #http_state{req=Req}) ->
    Req:file(filename:join(web_root(), "index.html"), [{"Content-Type", "text/html"}]);

handle('GET', ["index.html"], #http_state{req=Req}) ->
    Req:file(filename:join(web_root(), "index.html"), [{"Content-Type", "text/html"}]);

handle('GET',["favicon.ico"], #http_state{req=Req}) ->
    Req:file(filename:join(web_root(), "favicon.ico"), [{"Content-Type", "image/vnd.microsoft.icon"}]);

handle('GET', ["js", FileName], #http_state{req=Req}) ->
    Req:file(filename:join(sub_folder("js"), FileName), [{"Content-Type", "text/javascript"}]);

handle('GET', ["css", FileName], #http_state{req=Req}) ->
    Req:file(filename:join(sub_folder("css"), FileName), [{"Content-Type", "text/css"}]);

handle(_, _, #http_state{req=Req}) ->
    Req:ok([{"Content-Type", "text/plain"}], "Page not found.").

web_root() ->
    filename:join(code:priv_dir(rabbitmq_websockets), "www").

sub_folder(Folder) ->
    filename:join(web_root(), Folder).

%-----------------------------------
% Websockets Handling Implementation
% ----------------------------------

handle_websocket(#websocket_state{ws=Ws, conn=Connection, handler={M, F}} = State) ->
    receive
        {amqp_msgs, Msg} ->
            Ws:send(M:F(Msg)),
            handle_websocket(State);
        {browser, Data} ->
            {Ex, RKey} = parse_data(Data),
            error_logger:info_msg("Binding to exchange: ~p with RKey: ~p~n", [Ex, RKey]),
            maybe_stop_consumer(State#websocket_state.consumer),
            {ok, Channel}  = amqp_connection:open_channel(Connection),
            {ok, Consumer} = rabbit_websockets_consumer:start([Channel, Ex, RKey, self()]),
            handle_websocket(State#websocket_state{consumer = Consumer});
        closed ->
            maybe_stop_consumer(State#websocket_state.consumer);
        _Ignore ->
            handle_websocket(State)
    after 5000 ->
            handle_websocket(State)
    end.

parse_data(Data) ->
    case string:tokens(Data, ":") of
        [Exchange, RKey] -> {list_to_binary(Exchange), list_to_binary(RKey)};
        [Exchange] -> {list_to_binary(Exchange), <<"">>}
    end.

maybe_start_consumer(undefined, Args) ->
    {ok, Pid} = rabbit_websockets_consumer:start(Args),
    Pid;
maybe_start_consumer(Consumer, _) when is_pid(Consumer) ->
    Consumer.

maybe_stop_consumer(undefined) ->
    ok;
maybe_stop_consumer(Consumer) when is_pid(Consumer) ->
    rabbit_websockets_consumer:stop(Consumer).