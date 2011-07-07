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
    var exports = {};
    exports.createServer = function(cls, handler, options) {
      if (cls.Processor) {
        cls = cls.Processor;
      }
      var processor = new cls(handler);
      var transport = options.transport;
      var protocol = options.protocol;
      var server = options.server;

      return new server(function(stream) {
        // Note: it's a little unfortunate that socket.io uses 'message' while
        // node.js uses 'data' for the same event. I have not yet found a good
        stream.on(server.receive_event_name(), transport.receiver(function(transport_with_data) {
          var input = new protocol(transport_with_data);
          var output = new protocol(new transport(undefined, function(buf) {
            stream.write(buf);
          }));

          try {
            processor.process(input, output);
            transport_with_data.commitPosition();
          }
          catch (e) {
            if (e instanceof ttransport.InputBufferUnderrunError) {
              transport_with_data.rollbackPosition();
            }
            else {
              throw e;
            }
          }
        }));

        stream.on('end', function() {
          stream.end();
        });
      });
    };
    return exports;
});


