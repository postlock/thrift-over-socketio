-module(demo).
-include_lib("../include/socketio.hrl").
-compile(export_all).
-behaviour(gen_event).

main() ->
    application:start(sasl),
    application:start(misultin),
    application:start(socketio),
    {ok, Pid} = socketio_listener:start([{http_port, 7878}, 
                                         {default_http_handler,?MODULE}]),
    EventMgr = socketio_listener:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[]).

%% gen_event
init([]) ->
    {ok, undefined}.

handle_event({client, Pid}, State) ->
    io:format("Connected: ~p~n",[Pid]),
    EventMgr = socketio_client:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[]),
    {ok, State};
handle_event({disconnect, Pid}, State) ->
    io:format("Disconnected: ~p~n",[Pid]),
    {ok, State};
handle_event({message, Client, #msg{ content = Content } = Msg}, State) ->
    io:format("Got a message: ~p from ~p~n",[Msg, Client]),
    ProtoGen = fun() ->
                       {ok, SocketioTransport} = thrift_socketio_transport:new(Client, list_to_binary(Content)),
                       thrift_json_protocol:new(SocketioTransport)
               end,
    thrift_processor:init({self(), ProtoGen, calculator_thrift, server}),
    {ok, State};

handle_event(E, State) ->
    {ok, State}.

handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%
resource_to_dir("js") -> "/home/postlock/git/thrift-over-socketio/src/js/";
resource_to_dir("genjs") -> "/home/postlock/git/thrift-over-socketio/gen/js/";
resource_to_dir("shared_types.js") -> "/home/postlock/git/thrift-over-socketio/gen/js/shared_types.js";
resource_to_dir("tutorial_types.js") -> "/home/postlock/git/thrift-over-socketio/gen/js/tutorial_types.js";
resource_to_dir("Calculator.js") -> "/home/postlock/git/thrift-over-socketio/gen/js/Calculator.js";
resource_to_dir("SharedService.js") -> "/home/postlock/git/thrift-over-socketio/gen/js/SharedService.js";

resource_to_dir(_) -> none.
handle_request('GET', [], Req) ->
    Req:file("/home/postlock/git/thrift-over-socketio/priv/index.html");
handle_request('GET', [File]=Path, Req) ->
    case resource_to_dir(File) of
        none -> 
            io:format("**** file not found ~p~n", [Path]),
            Req:respond(200); % should be 404 actually, but whatever
        F ->
            io:format("**** serving file ~p~n", [F]),
            Req:file(F)
    end;

handle_request('GET', [Resource|File]=Path, Req) ->
    case resource_to_dir(Resource) of
        none -> 
            io:format("**** file not found ~p~n", [Path]),
            Req:respond(200); % should be 404 actually, but whatever
        Dir ->
            FullPath = Dir ++ string:join(File, "/"),
            io:format("**** serving file ~p~n", [FullPath]),
            Req:file(FullPath)
    end;
handle_request(_Method, _Path, Req) ->
    Req:respond(200).
