xser - it server
======
test 

Run
-----
rebar compile

erl -pa ebin

application:start(xser).

Test
-----
rebar compile

erl -pa ebin

eunit:test(xser).


Client
--------
[xcli](https://github.com/paladim/xcli).

