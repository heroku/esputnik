-module(esputnik_first_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%% Tests to run %%%
%%%%%%%%%%%%%%%%%%%%
%% Specific test cases or groups to run. The test case is named as
%% a single atom. Groups are named as {group, GroupName}. The tests
%% will run in the order given in the list.
all() ->
    [
     active_alert,
     active_resolve,
     alert_opts,
     esputnik_server
    ].

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    ok = application:start(meck),
    [{server, <<"test">>}|Config].

%% Runs once at the end of the suite. The process is different
%% from the one the case will run in.
end_per_suite(Config) ->
    application:stop(meck),
    Config.

%% Runs before the test case. Runs in the same process.
init_per_testcase(active_alert, Config) ->
    meck:new(hackney, [unstick, passthrough]),
    meck:expect(hackney, request,
                fun(post, <<"test/alert">>, [], {form, FormData}) ->
                        <<"dev">> = proplists:get_value(<<"team">>, FormData),
                        <<"active_alert">> = proplists:get_value(<<"message">>, FormData),
                        <<"alert">> = proplists:get_value(<<"type">>, FormData),
                        {ok, 200, [], connection1}
                end),
    meck:expect(hackney, send_request,
                fun(connection2, _) ->
                        {error, closed}
                end),
    meck:expect(hackney, body,
                fun(connection1) ->
                        {ok, <<"{\"request_id\":\"random\"}">>, connection2}
                end),
    meck:expect(hackney, close,
                fun(active_client) ->
                        ok
                end),
    Config;
init_per_testcase(active_resolve, Config) ->
    meck:new(hackney, [unstick, passthrough]),
    meck:expect(hackney, request,
                fun(post, <<"test/alert">>, [], {form, FormData}) ->
                        <<"dev">> = proplists:get_value(<<"team">>, FormData),
                        <<"active_resolve">> = proplists:get_value(<<"message">>, FormData),
                        <<"resolve">> = proplists:get_value(<<"type">>, FormData),
                        {ok, 200, [], connection1}
                end),
    meck:expect(hackney, body,
                fun(connection1) ->
                        {ok, <<"{\"request_id\":\"random\"}">>, connection2}
                end),
    meck:expect(hackney, close,
                fun(connection2) ->
                        ok
                end),
    Config;
init_per_testcase(alert_opts, Config) ->
    meck:new(hackney, [unstick, passthrough]),
    meck:expect(hackney, request,
                fun(post, <<"test/alert">>, [], {form, FormData}) ->
                        <<"dev">> = proplists:get_value(<<"team">>, FormData),
                        <<"alert_opts">> = proplists:get_value(<<"message">>, FormData),
                        <<"alert">> = proplists:get_value(<<"type">>, FormData),
                        <<"test">> = proplists:get_value(<<"request_id">>, FormData),
                        MessageId = proplists:get_value(<<"message_id">>, FormData),
                        <<"critical">> = proplists:get_value(<<"priority">>, FormData),
                        meck:expect(hackney, body,
                                    fun(connection1) ->
                                            {ok, <<"{\"request_id\":\"", MessageId/binary  ,"\"}">>, connection2}
                                    end),
                        {ok, 200, [], connection1}
                end),
    meck:expect(hackney, close,
                fun(connection2) ->
                        ok
                end),
    Config;
init_per_testcase(esputnik_server, Config) ->
    ets:new(esputnik_server, [named_table, public]),
    meck:new(hackney, [unstick, passthrough]),
    meck:expect(hackney, request,
                fun(post, <<"test/alert">>, [], {form, FormData}) ->
                        Team = proplists:get_value(<<"team">>, FormData),
                        Message = proplists:get_value(<<"message">>, FormData),
                        <<"alert">> = proplists:get_value(<<"type">>, FormData),
                        ets:insert(esputnik_server, {Team, Message}),
                        {ok, 200, [], connection1}
                end),
    meck:expect(hackney, send_request,
                fun(connection2, {post, <<"/alert">>, [], {form, FormData}}) ->
                        Team = proplists:get_value(<<"team">>, FormData),
                        <<"alert">> = proplists:get_value(<<"type">>, FormData),
                        case proplists:get_value(<<"message">>, FormData) of
                            <<"esputnik_server1">>=Message ->
                                ets:insert(esputnik_server, {Team, Message}),
                                {ok, 200, [], connection1};
                            <<"esputnik_server2">> ->
                                {error, closed}
                        end
                end),
    meck:expect(hackney, body,
                fun(connection1) ->
                        {ok, <<"{\"request_id\":\"random\"}">>, connection2}
                end),
    meck:expect(hackney, close,
                fun(active_client) ->
                        ok
                end),
    Config;
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(esputnik_server, Config) ->
    ets:delete(esputnik_server),
    meck:unload(hackney),
    Config;
end_per_testcase(_CaseName, Config) ->
    meck:unload(hackney),
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
active_alert(Config) ->
    Server = ?config(server, Config),
    {ok, <<"random">>, connection2} = esputnik_api:alert(Server, alert, <<"dev">>, <<"active_alert">>),
    Package = esputnik_api:to_sputnik_message(alert, <<"dev">>, <<"active_alert">>, []),
    {ok, <<"random">>, connection2} = esputnik_api:send_alert(Server, Package),
    {error, closed} = esputnik_api:send_alert(connection2, Package),
    Config.

active_resolve(Config) ->
    Server = ?config(server, Config),
    {ok, <<"random">>, connection2} = esputnik_api:alert(Server, resolve, <<"dev">>, <<"active_resolve">>),
    Config.

alert_opts(Config) ->
    Server = ?config(server, Config),
    MessageId = <<"msg_id">>,
    {ok, MessageId, connection2} = esputnik_api:alert(Server, alert, <<"dev">>, <<"alert_opts">>,
                                                      [{request_id, <<"test">>},
                                                       {message_id, MessageId},
                                                       {priority, critical}]),
    Config.

esputnik_server(Config) ->
    Server = ?config(server, Config),
    {ok, _Pid} = esputnik:start_link(Server),
    ok = esputnik:alert(alert, <<"team1">>, <<"esputnik_server">>),
    <<"esputnik_server">> = proplists:get_value(<<"team1">>, wait_for_message(1)),
    ok = esputnik:alert(alert, <<"team2">>, <<"esputnik_server1">>),
    <<"esputnik_server1">> = proplists:get_value(<<"team2">>, wait_for_message(2)),
    ok = esputnik:alert(alert, <<"team3">>, <<"esputnik_server2">>),
    <<"esputnik_server2">> = proplists:get_value(<<"team3">>, wait_for_message(3)),
    Config.

wait_for_message(Length) ->
    case ets:tab2list(esputnik_server) of 
        List when length(List) == Length ->
            List;
        _ ->
            wait_for_message(Length)
    end.
