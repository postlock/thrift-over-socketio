// For the connection URL, use the one which points to the java server written for the official javascript tutorial.
// Serving files and AJAX requests on different ports works with google-crome --disable-web-security
define(
    [// dependencies for the demo:
    // thrift-related
    "thrift", 
    "socketio_transport",
    "json_protocol",
    // jquery for the ui stuff
    "lib/jquery-1.6.2"], function(thrift, socketio_transport, json_protocol) {
        require.ready(function() {
            var genjs_files = ["./shared_types", "./SharedService", "./tutorial_types", "./Calculator"],
                genjs_modules = {
                    'thrift': thrift
                },
                show_ex = function(msg) {
                    $('#result').val(msg);
                    $('#result').css('color', 'red');
                },
                calc_cb = function(exception, result) {
                    if (exception !== null) {
                        if (exception.why && exception.why.value) {
                            show_ex(exception.why.value);
                        }
                        if (exception.message && exception.message.value) {
                            show_ex(exception.message.value);
                        }
                    } else {
                        $('#result').val(result.value);
                        $('#result').css('color', 'black');
                    }
                },
                transport,
                client,
                recv_cb = function () {
                    transport.recv(client);
                },
                socket = new io.Socket(location.hostname),
                calc = function () {
                    var work;
                    work = new Work();
                    work.num1 = $("#num1").val();
                    work.num2 = $("#num2").val();
                    work.op = $("#op").val();

                    try {
                        result = client.calculate(1, work, calc_cb);
                    } catch(ouch){
                        show_ex(ouch.why);
                    }
                },
                auto_calc = function () {
                    if ($('#autoupdate:checked').val() !== undefined) {
                        calc();
                    }
                },
                init = function() {
                    $("#op").children().remove();
                    // add operations to it's dropdown menu
                    $.each(Operation, function(key, value) {
                        $('#op').append($("<option></option>").attr("value",value).text(key)); 
                    });
                    $('table.calculator').attr('width', 500);
                    $('#num1').keyup(auto_calc);
                    $('#op').keyup(auto_calc);
                    $('#num2').keyup(auto_calc);
                    $('#calculate').click(calc);
                    transport = new socketio_transport.TSocketioTransport(socket, recv_cb);
                    client = new CalculatorClient(transport, json_protocol.TJSONProtocol);
                    socket.connect();
                },
                // ---- library functions below ----
                merge = function (src, dest) {
                    var i;
                    for (i in src) if (src.hasOwnProperty(i)) dest[i] = src[i];
                    return dest;
                },
                get_module_name = function(filename) {
                    // based on: http://phpjs.org/functions/basename:360
                    return filename.replace(/^.*[\/\\]/g, '').split('\.',1);
                },
                fake_require,
                real_require,
                load_genjs_file = function(jsfile, cb) {
                    // hide the "real" require object!
                    var real_require = require,
                        module_name = get_module_name(jsfile),
                        result = {},
                        module = {},
                        exports = {};
                    result[module_name] = {};
                    // sometimes stuff is added to module directly,
                    // other times it is iadded to module.exports.
                    window.exports = exports;
                    window.module = module;
                    real_require([jsfile], function() {
                        if (typeof(cb) === 'function') {
                            // we need define so require.js finds the module.
                            merge(exports, result[module_name]);
                            if (module.hasOwnProperty('exports')) {
                                merge(module.exports, result[module_name]);
                            }
                            cb(result);
                        }
                    });
                },
                globalize_modules = function(module_store)  {
                    var i,j;
                    for (i in module_store) {
                        if (module_store.hasOwnProperty(i)) {
                            for (j in module_store[i]) {
                                if (module_store[i].hasOwnProperty(j)) {
                                    window[j] = module_store[i][j];
                                }
                            }
                        }
                    }
                },
                load_all_genjs_files; 
        
            fake_require = function() {
                if (arguments.length === 1 && typeof(arguments[0]) === 'string') return genjs_modules[get_module_name(arguments[0])];
                return real_require.apply(this, arguments);
            };
            merge(real_require, fake_require);
            real_require = window.require;
            window.require = fake_require;

            load_all_genjs_files = function(file_list, module_store, on_ready) {
                var current_file = file_list.shift(), module;
                // If we have reached the end of the list, end the recursion.
                if (current_file === undefined) {
                    globalize_modules(module_store);
                    window.require = real_require;
                    console.log("finished loading generated .js files");
                    return on_ready.apply(this, []);
                }
                load_genjs_file(current_file, function(module) {
                    merge(module, module_store);
                    load_all_genjs_files(file_list, module_store, on_ready);
                }); 
            };
            load_all_genjs_files(genjs_files, genjs_modules, init);

            //socket.on('connect', function() {
            //    console.log("socket.io connectd, loading generated .js files");
            //});
        });
});
/*
   function init() {
   socket = new io.Socket(location.hostname);
   socket.on('message', function(data){
   console.log(data);
   });
   socket.on('connect', function(){
   console.log("Imma connectad!");
   console.log(socket);
   socket.send({msg: "Erlang rulez!"});

   });
   socket.connect();
   }*/

