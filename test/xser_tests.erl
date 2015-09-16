-module(xser_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).


start_stop_test_() ->
    [
        {"Reg name1",           ?setup(fun reg_name1/1)},
        {"Get name(empty)",     ?setup(fun get_i_empty/1)},
        {"Get list(empty)",     ?setup(fun get_list_empty/1)},
        {"Send msg(no)",        ?setup(fun send_msg_no/1)},
        {"Get history(empty)",  ?setup(fun get_history_empty/1)}
    ].


% simple test
start_server_test() -> 
    {ok, Pid} = xser:start_link(),
    Fun = fun () -> test_reg_new_user() end,

    ?assertEqual(true, is_pid(Pid)),

    ?assertMatch({i, no}, gen_server:call({global, xser},i)),
    ?assertMatch({list_users, []}, gen_server:call({global, xser},get_list_users)),
    ?assertMatch({name, {user, "Name1", _Pid, 1, up}}, gen_server:call({global, xser},{reg_name, "Name1"})),
    ?assertMatch({i, {user, "Name1", _Pid, 1, up}}, gen_server:call({global, xser},i)),

    spawn(Fun),
    Data = receive 
        {msg, _From, D} -> D 
            after 5000 -> 
                false 
        end,
    ?assertEqual(Data, "Hello"),
    timer:sleep(1000),
    ?assertMatch({send_msg, no}, gen_server:call({global, xser},{send_msg, "Name2", "Hello"})),
    ?assertMatch({history,[{"Name2","Hello"}]}, gen_server:call({global, xser},{get_history, "Name2"})),
    ?assertMatch(
        {list_users, 
            [
                {user, "Name1", _, 1, up},
                {user, "Name2", _, 2, down}
            ]
        }, gen_server:call({global, xser},get_list_users)).

% helpers
test_reg_new_user() ->
    ?assertMatch({name, {user, "Name2", _Pid, 2, up}}, gen_server:call({global, xser},{reg_name, "Name2"})),
    ?assertMatch({name, no}, gen_server:call({global, xser},{reg_name, "Name1"})),
    ?assertMatch({name, {user, "Name2", _Pid, 2, up}}, gen_server:call({global, xser},{reg_name, "Name2"})),
    ?assertMatch({send_msg, ok}, gen_server:call({global, xser},{send_msg, "Name1", "Hello"})).

start() ->
    {ok, Pid} = xser:start_link(),
    Pid.

stop(Pid) -> 
    exit(Pid, normal).

reg_name1(_) ->
    ?_assertMatch({name, {user, "Name1", _Pid, 1, up}}, gen_server:call({global, xser},{reg_name, "Name1"})).

get_i_empty(_) ->
    ?_assertMatch({i, no}, gen_server:call({global, xser},i)).

get_list_empty(_) ->
    ?_assertMatch({list_users, []}, gen_server:call({global, xser},get_list_users)).

get_history_empty(_) ->
    ?_assertMatch({history,[]}, gen_server:call({global, xser},{get_history, "Name2"})).

send_msg_no(_) ->   
    ?_assertMatch({send_msg, no}, gen_server:call({global, xser},{send_msg, "Name2", "Hello"})).
