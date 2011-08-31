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

-export([new/2,
         write/2, read/2, flush/1, close/1]).

-record(data, {client_pid,
               content,
               recv_timeout=infinity}).
-type state() :: #data{}.
-include("thrift_transport_behaviour.hrl").

new(ClientPid, Content) ->
    State = #data{
        client_pid = ClientPid,
        content = Content
    },
    thrift_transport:new(?MODULE, State).

%% Data :: iolist()
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

read(This = #data{content = <<>>}, _Len) ->
    {This, {error, 'EOF'}};

%% TODO: handle the case where Len > length of remaining binary
read(This = #data{content=Content}, Len) ->
    <<Read:Len/binary,Rest/binary>> = Content, 
    {
        This#data{content=Rest},
        {ok, Read}
    }.

%% We can't really flush - everything is flushed when we write
flush(This = #data{client_pid = ClientPid, content = Content}) ->
    {This#data{content = <<>> },  socketio_client:send(ClientPid, #msg{ content = binary_to_list(iolist_to_binary(lists:reverse(Content)))})}.

%% Cant really close - maybe disconnect?
close(This) ->
    io:format("thrift_socketio_transport:close called~n", []),
    {This, ok}.

