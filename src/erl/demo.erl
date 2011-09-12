-module(demo).
-include_lib("../include/socketio.hrl").
-include("calculator_thrift.hrl").
-compile(export_all).
-behaviour(gen_event).
-record(state, {
          % tree containing pending requests
          pending_requests = gb_trees:empty()
          }).

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
    io:format("Connected: ~p (self is ~p)~n ",[Pid, self()]),
    EventMgr = socketio_client:event_manager(Pid),
    %ok = gen_event:add_handler(EventMgr, ?MODULE,[]),
    ok = gen_event:add_handler(EventMgr, socketio_thrift_shim,[Pid, calculator_thrift, server]),
    {ok, State};
handle_event({disconnect, Pid}, State) ->
    io:format("Disconnected: ~p~n",[Pid]),
    {ok, State};
%handle_event({message, Client, #msg{ content = Content } = Msg}, State) ->
%    {ok, State};

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

new_client(Pid, Service, _Options) ->
    {ProtoOpts, TransOpts} = {[],[]},
    TransportFactory = fun() -> thrift_socketio_transport:new(Pid, TransOpts) end,
    {ok, ProtocolFactory} = thrift_json_protocol:new_protocol_factory(
                              TransportFactory, ProtoOpts),
    {ok, Protocol} = ProtocolFactory(),
    thrift_client:new(Protocol, Service).

t(Pid) ->
    {ok, Client0} = new_client(Pid, calculator_thrift, []),
    {Client1, {ok, ok}} = thrift_client:call(Client0, ping, []),
    io:format("ping~n", []),

    {Client2, {ok, Sum}} = thrift_client:call(Client1, add,  [1, 1]),
    io:format("1+1=~p~n", [Sum]),

    {Client3, {ok, Sum1}} = thrift_client:call(Client2, add, [1, 4]),
    io:format("1+4=~p~n", [Sum1]),

    Work = #work{op=?tutorial_Operation_SUBTRACT,
                 num1=15,
                 num2=10},
    {Client4, {ok, Diff}} = thrift_client:call(Client3, calculate, [1, Work]),
    io:format("15-10=~p~n", [Diff]),

    {Client5, {ok, Log}} = thrift_client:call(Client4, getStruct, [1]),
    io:format("Log: ~p~n", [Log]),

    Client6 =
        try
            Work1 = #work{op=?tutorial_Operation_DIVIDE,
                          num1=1,
                          num2=0},
            {ClientS1, {ok, _Quot}} = thrift_client:call(Client5, calculate, [2, Work1]),

            io:format("LAME: exception handling is broken~n", []),
            ClientS1
        catch
            throw:{ClientS2, Z} ->
                io:format("Got exception where expecting - the " ++
                          "following is NOT a problem!!!~n~p~n", [Z]),
                ClientS2
        end,


    {Client7, {ok, ok}} = thrift_client:call(Client6, zip, []),
    io:format("zip~n", []),

    {_Client8, ok} = thrift_client:close(Client7),
    ok.
