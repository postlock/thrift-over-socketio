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
    TSocketioTransport = exports.TSocketioTransport = function(socket, recv_cb, server) {
        var transport_obj = this;
        this.server = server;
        this.socket = socket;
        this.wpos = 0;
        this.rpos = 0;

        this.send_buf = '';
        this.recv_buf = '';
        // register recieve message callback fun
        this.socket.on('message', function(data) {
            console.log(data);
            transport_obj.recv_buf = data;
            recv_cb(data);
        });
        this.socket.on('connect', function(){
            console.log(socket);
            // I tried, and service composition does work in the erlang server
            // socket.send('[1,"getStruct",1,4,{"1":{"i32":1}}]');
        });
    };

    TSocketioTransport.prototype = {

        flush: function() {
            var res = this.socket.send(this.send_buf);
            this.send_buf = "";
            return res;
        },

        recv: function (client) {
            var header, proto = new client.pClass(this);
            try {
                header = proto.readMessageBegin();
                /*
                client._reqs[dummy_seqid] = function(err, success){
                    transport_with_data.commitPosition();
                    var callback = client._reqs[header.rseqid];
                    delete client._reqs[header.rseqid];
                    if (callback) {
                        callback(err, success);
                    }
                };*/
                if (header.mtype === Thrift.MessageType.CALL) {
                    this.server.process(proto, proto);
                } else {
                    client['recv_' + header.fname].apply(client, [proto, header.mtype, header.rseqid]);
                }
            } catch (e) {
                var ex = e;
                if (!(ex instanceof TException)) {
                    ex = new Thrift.TApplicationException(Thrift.TApplicationExceptionType.UNKNOWN, 'An unknown error occured');
                } 
                // note that this assumes MessageBegin was successfully parsed
                proto.writeMessageBegin(header.fname, Thrift.MessageType.EXCEPTION, header.rseqid);
                ex.write(proto);
                proto.writeMessageEnd();
                proto.flush();
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
            this.send_buf = this.send_buf + buf;
        },

        getSendBuffer: function() {
            return this.send_buf;
        }

    };
    return exports;
}); // end define
