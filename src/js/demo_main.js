require([ // dependencies for the demo:
    // thrift-related
    "thrift", 
    "socketio_transport",
    // jquery for the ui stuff
    "lib/jquery-1.6.2"], function(thrift, socketio_transport) {
        console.dir(arguments);
        require.ready(function() {
            var generated = {},
                socket = new io.Socket(location.hostname),
                calc = function () {
                    var transport = new Thrift.Transport("socket");
                    var protocol  = new Thrift.Protocol(transport);
                    var client    = new CalculatorClient(protocol);

                    var work = new Work()
                    work.num1 = $("#num1").val();
                    work.num2 = $("#num2").val();
                    work.op = $("#op").val();

                    try {
                        result = client.calculate(1, work);
                        $('#result').val(result);
                        $('#result').css('color', 'black');
                    } catch(ouch){
                        $('#result').val(ouch.why);
                        $('#result').css('color', 'red');
                    }
                },
                auto_calc = function () {
                    if ($('#autoupdate:checked').val() !== undefined) {
                        calc();
                    }
                },
                load_generated = function() {
                    // hide the "real" require object!
                    var real_require = require;
                    window.exports = generated;
                    window.Thrift = thrift;
                    // if require is used as it is used in nodejs, it returns the same object.
                    require = function() {return generated;};
                    real_require(["/genjs/Calculator.js"], function() {
                        console.dir(arguments);
                        init();
                    });
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
                };

            load_generated();
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

