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
    TSocketioTransport = exports.TSocketioTransport = function(socket, recv_cb) {
        var transport_obj = this;
        this.socket = socket;
        this.wpos = 0;
        this.rpos = 0;

        this.send_buf = '';
        this.recv_buf = '';
        // register recieve message callback fun
        this.socket.on('message', function(data) {
            transport_obj.recv_buf = data;
            recv_cb(data);
        });
        this.socket.on('connect', function(){
            console.log("Imma connectad!");
            console.log(socket);
        });
    };

    TSocketioTransport.prototype = {

        flush: function() {
            return this.socket.send(this.send_buf);
        },

        recv: function (client) {
            var message = new client.pClass(this);
            try {
                var header = message.readMessageBegin();
                /*
                client._reqs[dummy_seqid] = function(err, success){
                    transport_with_data.commitPosition();
                    var callback = client._reqs[header.rseqid];
                    delete client._reqs[header.rseqid];
                    if (callback) {
                        callback(err, success);
                    }
                };*/
                client['recv_' + header.fname].apply(client, [message, header.mtype, header.rseqid]);
            } catch (e) {
                console.log(e.stack);
            }
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
