## COMPILING THE DEMO
erlc -I ~/git/socket.io-erlang/include -o ~/git/thrift-over-socketio/ebin demo.erl

## RUNNING THE DEMO
erl -pa ~/git/thrift/lib/erl/ebin -pa ~/git/socket.io-erlang/deps/jsx/ebin -pa \
 ~/git/socket.io-erlang/deps/misultin/ebin/ -pa ~/git/socket.io-erlang/ebin -pa \
 ~/git/thrift-over-socketio/ebin

## TRACING WITH REDBUG
redbug:start(500000,10,{demo,handle_request,[{'GET','_','_'},stack,return]}).
