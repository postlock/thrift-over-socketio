%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(thrift_socketio_transport).
-include_lib("../include/socketio.hrl").

-behaviour(thrift_transport).

-export([new/1, new/3,
         write/2, read/2, flush/1, close/1]).

-record(data, {shim_pid,
               content,
               expect_reply,
               recv_timeout=infinity}).
-type state() :: #data{}.
-include("thrift_transport_behaviour.hrl").
-define(TIMEOUT, infinity).
new(ClientPid) ->
    new(ClientPid, undefined, true).
new(ClientPid, Content, ExpectReply) ->
    State = #data{
        shim_pid = ClientPid,
        expect_reply = ExpectReply,
        content = Content
    },
    thrift_transport:new(?MODULE, State).

%% Data :: iolist()
write(This = #data{content = undefined}, Data) ->
    write(This#data{content = <<>>}, Data);
write(This = #data{content = <<>>}, Data) ->
    {
        This#data{content=[Data]}, 
        ok
    };
write(This = #data{content = Content}, Data) ->
    {
        This#data{content=[Data|Content]}, 
        ok
    }.

read(This = #data{content = undefined}, Len) ->
    % We are trying to read data that isn't here yet.
    % By sending out the request, socketio_thrift_shim knows its time to call us when the response arrives
    % so we wait for a while, then inform the shim that we've timed out.
    receive
        {reply, Content} ->
            %io:format("got reply which we waited on: ~p~n", [Content]),
            read(This#data{content=Content}, Len);
        Other ->
            io:format("got unexpected ~p in reply ~n", [Other])
    after ?TIMEOUT ->
        % TODO: inform shim that we're not waiting any more
        % TODO: we shouldn't return anything (since thrift_client cant handle errors),
        % instead, we should throw an exception.
        {This, {error, 'EOF'}}
    end;


read(This = #data{content = <<>>, expect_reply = ExpectReply}, _Len) ->
    {This#data{
        content = case ExpectReply of
            true -> 
                undefined;
            false ->
                <<>>
        end
    }, {error, 'EOF'}};

%% TODO: handle the case where Len > length of remaining binary
read(This = #data{content=Content}, Len) ->
    <<Read:Len/binary,Rest/binary>> = Content, 
    {
        This#data{content=Rest},
        {ok, Read}
    }.

flush(This = #data{shim_pid = Pid, content = Content, expect_reply = ExpectReply}) ->
    gen_event:notify(Pid, {send, iolist_to_binary(lists:reverse(Content)), self()}),
    % we set content to undefined so if there is a read after this flush,
    % we will wait for the response to our request.
    {This#data{
        content = case ExpectReply of
            true -> 
                undefined;
            false ->
                <<>>
        end
    }, ok}.

%% Cant really close - maybe disconnect?
close(This) ->
    io:format("thrift_socketio_transport:close called~n", []),
    {This#data{content = undefined}, ok}.

