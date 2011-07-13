/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
define(function () {
    /**
     *If you do not specify a url then you must handle ajax on your own.
     *This is how to use js bindings in a async fashion.
     */
    var exports = {}, TSocketioTransport;
    TSocketioTransport = exports.TSocketioTransport = function(socket) {
        this.socket = socket;
        this.wpos = 0;
        this.rpos = 0;

        this.send_buf = '';
        this.recv_buf = '';
        // register recieve message callback fun
        socket.on('message', function(data) {
            this.recv_buf = data;
        });
    };

    TSocketioTransport.prototype = {

        flush: function() {
            return this.send_buf;
        },

        isOpen: function() {
            return true;
        },

        open: function() {},

        close: function() {},

        read: function(len) {
            var avail = this.wpos - this.rpos;

            if (avail === 0) {
                return '';
            }

            var give = len;

            if (avail < len) {
                give = avail;
            }

            var ret = this.read_buf.substr(this.rpos, give);
            this.rpos += give;

            //clear buf when complete?
            return ret;
        },

        readAll: function() {
            return this.recv_buf;
        },

        write: function(buf) {
            this.send_buf = buf;
            this.socket.send(this.send_buf);
        },

        getSendBuffer: function() {
            return this.send_buf;
        }

    };
    SocketioServer = exports.SocketioServer = function(on_message_fun) {
        this.socketio_conn = undefined;
        this.on_message_fun = on_message_fun;
    };
    SocketioServer.prototype.listen = function(socketio_conn) {
        this.socketio_conn = socketio_conn;
        this.socketio_conn.on('message', this.on_message_fun);
    };
    SocketioServer.prototype.receive_event_name = function() {
        return "message";
    };

    return exports;
}); // end define
