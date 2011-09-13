-module(socketio_thrift_shim).
-include_lib("../include/socketio.hrl").
-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").
-compile(export_all).
-behaviour(gen_event).
-record(state, {
          % tree containing pending requests
          client,
          service,
          handler,
          pending
          }).

%% gen_event
init([ClientPid, Service, Handler]) ->
    {ok, #state{
        client = ClientPid,
        service = Service,
        handler = Handler,
        pending = gb_trees:empty()
    }}.

handle_event({message, _Client, #msg{ content = ListContent } = Msg}, 
        #state{
            service=Service,
            handler=Handler
        }=State) ->
    Content = list_to_binary(ListContent),
    io:format("socketio_thrift_shim received message: ~p (self is ~p)~n",[Msg, self()]),
    #protocol_message_begin{
        type = Type,
        seqid = Seqid} = get_message_begin(Content),
    State1 = case Type of 
        ?tMessageType_CALL ->
            % received an incoming call, we can pass this immediately to thrift_processor.
            process_incoming(Content, Service, Handler),
            State;
        ?tMessageType_ONEWAY ->
            % received an incoming call, we can pass this immediately to thrift_processor.
            process_incoming(Content, Service, Handler),
            State;
        ?tMessageType_REPLY ->
            % received a response to one of our requests. locate waiting process and forward the message
            forward_reply(Content, Seqid, State);
        ?tMessageType_EXCEPTION ->
            % received a response to one of our requests. locate waiting process and forward the message
            forward_reply(Content, Seqid, State)
    end,
    {ok, State1};

handle_event({send, Content, SenderPid}, #state{
            client=ClientPid
        }=State) ->
    io:format("sending message: ~p on behalf of ~p~n",[Content, SenderPid]),
    #protocol_message_begin{
        type = Type,
        seqid = Seqid} = get_message_begin(Content),
    State1 = case Type of 
        ?tMessageType_CALL ->
            % The sender will be waiting for a response to this request
            add_pending(SenderPid, Content, Seqid, State);
        _ ->
            State
    end,
    do_send(ClientPid, Content),
    {ok, State1}.

get_message_begin(Message) ->
    {ok, Transport} = thrift_socketio_transport:new(undefined, Message, false),
    {ok, Proto0} = thrift_json_protocol:new(Transport), 
    {_Proto1, MessageBegin} = thrift_protocol:read(Proto0, message_begin),
    MessageBegin.

process_incoming(Message, Service, Handler) ->
    % TODO: this should be happending in a separate process, which we should be linked to if it crashes.
    ProtoGen = fun() ->
        {ok, Transport} = thrift_socketio_transport:new(self(), Message, false),
        thrift_json_protocol:new(Transport)
    end,
    thrift_processor:init({undefined, ProtoGen, Service, Handler}).

forward_reply(ReplyContent, Seqid, #state{pending=Pending} = State) ->
    % we're assuming a request was made with this seqid
    {SenderPid, _ReqContent} = gb_trees:get(Seqid, Pending),
    % TODO: remove from Pending
    SenderPid ! {reply, ReplyContent},
    State.
    
do_send(ClientPid, Content) -> 
    socketio_client:send(ClientPid, #msg{ content = binary_to_list(Content)}).

add_pending(SenderPid, Content, Seqid, #state{pending=Pending0}=State) ->
    % we're assuming the seqid is not used as a key in the Pending tree
    Pending1 = gb_trees:insert(Seqid, {SenderPid, Content}, Pending0),
    State#state{pending=Pending1}.
    

