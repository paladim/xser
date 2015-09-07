xser - it server
======

Client
--------
[xcli](https://github.com/paladim/xcli).

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



