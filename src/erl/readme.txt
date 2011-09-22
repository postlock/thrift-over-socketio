## COMPILING THE DEMO
erlc  +debug +debug_info -I ~/git/socket.io-erlang/include -I ~/git/thrift/lib/erl/include -I ~/git/thrift-over-socketio/gen/erl/ -pa ~/git/socket.io-erlang/ebin/ -o ~/git/thrift-over-socketio/ebin *.erl
~/opt/bin/thrift -r -out /home/postlock/git/thrift-over-socketio/gen/erl --gen erl /home/postlock/git/thrift-over-socketio/src/thrift/tutorial.thrift
erlc -pa ~/git/thrift/lib/erl/ebin -o ~/git/thrift-over-socketio/ebin ~/git/thrift-over-socketio/gen/erl/*.erl

## RUNNING THE DEMO
erl -pa ~/git/thrift/lib/erl/ebin -pa ~/git/socket.io-erlang/deps/jsx/ebin -pa \
~/git/socket.io-erlang/deps/misultin/ebin/ -pa ~/git/socket.io-erlang/ebin -pa  ~/git/thrift-over-socketio/ebin -pa \
~/git/thrift/tutorial/erl/ebin/

## TRACING WITH REDBUG
redbug:start(500000,10,{demo,handle_request,[{'GET','_','_'},stack,return]}).

## JAVASCRIPT THRIFT CODE GENERATION
~/opt/bin/thrift -r -out /home/postlock/git/thrift-over-socketio/gen/js --gen js:node /home/postlock/git/thrift-over-socketio/src/thrift/tutorial.thrift
