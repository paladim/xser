-module(xser_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


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