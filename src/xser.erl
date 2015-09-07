-module(xser).

-behaviour(gen_server).

-compile(export_all).

-record(user, {name, pid = no, id = no, status = up}).
-define(T, get(tab)).
-define(ID, get(id)).
-include_lib("eunit/include/eunit.hrl").


start_link() ->
	net_kernel:start([xser@localhost, shortnames]),
	erlang:set_cookie(node(), 'Hello'),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Tab = ets:new(tab, [set]),
    put(tab, Tab),
    put(id, 0),
    {ok, []}.


%% callbacks

%%%%%%%%%%%%%%%% reg name
handle_call({reg_name, Name}, {FromPid, _Tag}, State) ->
    User = #user{name = Name, pid = FromPid, id = new_id(), status = up},
    {reply, {name, reg(User)}, State};

%%%%%%%%%%%%%%%% I
handle_call(i, {FromPid, _Tag}, State) ->
    {reply, {i, user_exist(FromPid)}, State};

%%%%%%%%%%%%%%%% get list users
handle_call(get_list_users, {_FromPid, _Tag}, State) ->
    List1 = ets:select(?T,[{{ {user, '_'}, '_' },[],['$_']}]),
    List = [User || { _, User } <- List1 ],
    {reply, {list_users, lists:reverse(List)}, State};

%%%%%%%%%%%%%%%% send message
handle_call({send_msg, ToName, Data}, {FromPid, _Tag}, State) ->
    Reply = send_msg(user_exist(ToName), user_exist(FromPid), Data),
    {reply, {send_msg, Reply}, State};

%%%%%%%%%%%%%%%% get history by name
handle_call({get_history, ToName}, {FromPid, _Tag}, State) ->
    ToUser = user_exist(ToName),
    FromUser = user_exist(FromPid),

    Val = case {is_tuple(ToUser), is_tuple(FromUser)} of
        {true, true} ->
            case history_exist((ToUser#user.id * FromUser#user.id) + ToUser#user.id + FromUser#user.id) of
                no -> [];
                ValList -> ValList
            end;
        _ -> []
    end,
    {reply, {history, Val}, State};

handle_call(get_pid_server, _From, State) ->
    {reply, {pid_server, self()}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({_Request, _From}, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    User = user_exist(Pid),
    if User =/= no -> user_down(User);
        true -> ok 
    end,
    erlang:demonitor(Ref),
    {noreply, State};

handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



reg(User) -> 
    User1 = user_exist(User#user.pid),
    if User1 =/= no -> user_down(User1); 
        true -> ok
    end,

    case user_exist(User#user.name) of
        no -> 
            true = ets:insert(?T,{ {user, User#user.name}, User}),
            erlang:monitor(process, User#user.pid),
            User;
        User2 -> case User2#user.status of
                up -> 
                    no;
                down -> 
                    User3 = User2#user{pid = User#user.pid},
                    user_up(User3),
                    erlang:monitor(process, User3#user.pid),
                    User3#user{status = up} 
            end
    end.


user_exist(Pid) when is_pid(Pid)->
    case ets:select(?T, [{ { {user, '_'} ,{user, '_', Pid, '_', '_'}}, [], ['$_'] }]) of
        []  -> no ;
        [{{user, _Name}, User} | _]  -> User
    end;
user_exist(Name) ->
    case ets:select(?T, [{ { {user, Name}, '_' }, [], ['$_'] }]) of
        []  -> no ;
        [{ {user, _Name}, User} | _]  -> User
    end. 
user_down(User) ->
    true = ets:insert(?T, { {user, User#user.name},User#user{pid = no, status = down} }).

user_up(User) ->
    true = ets:insert(?T,{ {user, User#user.name }, User#user{status = up}}).

send_msg(To, From, _) when (To == From) -> 
    no;
send_msg(To, From, _) when (To == no) or (From == no) -> 
    no;
send_msg(To, From, _) when (To#user.status == down) or (From#user.status == down) -> 
    no;
send_msg(To, From, Data) -> 
    To#user.pid ! {msg, From, Data},
    add_history(From, Data, (To#user.id * From#user.id) + To#user.id + From#user.id),
    ok.

add_history(From, Data, IdHistory) -> 
    case history_exist(IdHistory) of 
        no  -> ets:insert(?T,{ {history, IdHistory}, [{From#user.name, Data}] }); 
        Val -> 
            NewValList = [{From#user.name, Data} | Val],
            ets:insert(?T,{{history, IdHistory}, NewValList})
    end,
    ok.

history_exist(IdHistory) ->
    case ets:select(?T, [{ {{history, IdHistory}, '_'}, [], ['$_']   }]) of
        []  -> no ;
        [{{history, IdHistory}, Val} | _] -> Val
    end.

new_id() -> 
    NewId = get(id) + 1,
    put(id, NewId) + 1,
    NewId.



start_server_test() -> 
    {ok, Pid} = start_link(),
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

test_reg_new_user() ->
    ?assertMatch({name, {user, "Name2", _Pid, 2, up}}, gen_server:call({global, xser},{reg_name, "Name2"})),
    ?assertMatch({name, no}, gen_server:call({global, xser},{reg_name, "Name1"})),
    ?assertMatch({name, {user, "Name2", _Pid, 2, up}}, gen_server:call({global, xser},{reg_name, "Name2"})),
    ?assertMatch({send_msg, ok}, gen_server:call({global, xser},{send_msg, "Name1", "Hello"})).