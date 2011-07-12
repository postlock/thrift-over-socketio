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
    };

    TSocketioTransport.prototype = {

        flush: function(async) {
            //async mode
            if (async || this.url === undefined || this.url === '') {
                return this.send_buf;
            }

            var xreq = this.getXmlHttpRequestObject();

            if (xreq.overrideMimeType) {
                xreq.overrideMimeType('application/json');
            }

            xreq.open('POST', this.url, false);
            xreq.send(this.send_buf);

            if (xreq.readyState != 4) {
                throw 'encountered an unknown ajax ready state: ' + xreq.readyState;
            }

            if (xreq.status != 200) {
                throw 'encountered a unknown request status: ' + xreq.status;
            }

            this.recv_buf = xreq.responseText;
            this.recv_buf_sz = this.recv_buf.length;
            this.wpos = this.recv_buf.length;
            this.rpos = 0;
        },

        jqRequest: function(client, postData, args, recv_method) {
            if (typeof jQuery === 'undefined' ||
                typeof jQuery.Deferred === 'undefined') {
                throw 'Thrift.js requires jQuery 1.5+ to use asynchronous requests';
            }

            // Deferreds
            var deferred = jQuery.Deferred();
            var completeDfd = jQuery._Deferred();
            var dfd = deferred.promise();
            dfd.success = dfd.done;
            dfd.error = dfd.fail;
            dfd.complete = completeDfd.done;

            var jqXHR = jQuery.ajax({
                url: this.url,
                data: postData,
                type: 'POST',
                cache: false,
                dataType: 'text',
                context: this,
                success: this.jqResponse,
                error: function(xhr, status, e) {
                    deferred.rejectWith(client, jQuery.merge([e], xhr.tArgs));
                },
                complete: function(xhr, status) {
                    completeDfd.resolveWith(client, [xhr, status]);
                }
            });

            deferred.done(jQuery.makeArray(args).pop()); //pop callback from args
            jqXHR.tArgs = args;
            jqXHR.tClient = client;
            jqXHR.tRecvFn = recv_method;
            jqXHR.tDfd = deferred;
            return dfd;
        },

        jqResponse: function(responseData, textStatus, jqXHR) {
          this.setRecvBuffer(responseData);
          try {
              var value = jqXHR.tRecvFn.call(jqXHR.tClient);
              jqXHR.tDfd.resolveWith(jqXHR, jQuery.merge([value], jqXHR.tArgs));
          } catch (ex) {
              jqXHR.tDfd.rejectWith(jqXHR, jQuery.merge([ex], jqXHR.tArgs));
          }
        },

        setRecvBuffer: function(buf) {
            this.recv_buf = buf;
            this.recv_buf_sz = this.recv_buf.length;
            this.wpos = this.recv_buf.length;
            this.rpos = 0;
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
