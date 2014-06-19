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
    [{group, normal},
     {group, no_api}
    ].

groups() ->
    [{normal, [], [active_alert
                  ,active_resolve
                  ,alert_opts
                  ,esputnik_server
                  ,sputnik_api_timeout
                  ,sputnik_api_timeout_server
                  ,sputnik_server_throttle]},
     {no_api, [], [no_api_set]}
    ].
    

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    Config.

%% Runs once at the end of the suite. The process is different
%% from the one the case will run in.
end_per_suite(Config) ->
    Config.

%% Init per group
init_per_group(normal, Config) ->
    ok = application:start(meck),
    Server = <<"test">>,
    ok = application:set_env(esputnik, sputnik_api_url, Server),
    ok = error_logger:add_report_handler(esputnik_event_handler),
    esputnik_app:start(),
    [{server, Server}|Config];
init_per_group(no_api, Config) ->
    ok = application:unset_env(esputnik, sputnik_api_url),
    ok = error_logger:add_report_handler(esputnik_event_handler),
    esputnik_app:start(),
    Config.

end_per_group(normal, Config) ->
    application:stop(meck),
    error_logger:delete_report_handler(esputnik_event_handler),
    application:stop(esputnik),
    Config;
end_per_group(_, Config) ->
    error_logger:delete_report_handler(esputnik_event_handler),
    application:stop(esputnik),
    Config.

%% Runs before the test case. Runs in the same process.
init_per_testcase(active_alert, Config) ->
    meck:new(hackney, [unstick, passthrough]),
    meck:expect(hackney, request,
                fun(post, <<"test/alert">>, [], {form, FormData}, _) ->
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
                fun(post, <<"test/alert">>, [], {form, FormData}, _) ->
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
                fun(post, <<"test/alert">>, [], {form, FormData}, _) ->
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
    Server = ?config(server, Config),
    ok = application:set_env(esputnik, sputnik_api_url, Server),
    ok = application:set_env(esputnik, throttle_time, 0),
    ets:new(esputnik_server, [named_table, public]),
    meck:new(hackney, [unstick, passthrough]),
    meck:expect(hackney, request,
                fun(post, <<"test/alert">>, [], {form, FormData}, _) ->
                        Team = proplists:get_value(<<"team">>, FormData),
                        Message = proplists:get_value(<<"message">>, FormData),
                        <<"alert">> = proplists:get_value(<<"type">>, FormData),
                        ets:insert(esputnik_server, {Team, Message}),
                        {ok, 200, [], connection1};
                   (post, <<"new_url/alert">>, [], {form, FormData}, _) ->
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
                            <<"esputnik_server2">>=Message ->
                                ets:insert(esputnik_server, {Team, Message}),
                                {ok, 200, [], connection1};
                            <<"esputnik_server3">> ->
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
init_per_testcase(sputnik_api_timeout_server, Config) ->
    esputnik:change_api_url(<<"https://heroku.com">>),
    Config;
init_per_testcase(sputnik_server_throttle, Config) ->
    esputnik:change_api_url(<<"test">>),
    application:stop(esputnik),
    application:start(esputnik),
    application:set_env(esputnik, throttle_time, 1000),
    meck:new(hackney, [unstick, passthrough]),
    meck:expect(hackney, request,
                fun(post, <<"test/alert">>, [], {form, _FormData}, _) ->
                        {ok, 200, [], connection1}
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
    catch meck:unload(hackney),
    Config;
end_per_testcase(_CaseName, Config) ->
    catch meck:unload(hackney),
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
active_alert(Config) ->
    Server = ?config(server, Config),
    Message = esputnik_api:to_sputnik_message(alert, <<"dev">>, <<"active_alert">>),
    {ok, <<"random">>, connection2} = esputnik_api:send_alert(Server, Message),
    Message1 = esputnik_api:to_sputnik_message(alert, <<"dev">>, <<"active_alert">>, []),
    {ok, <<"random">>, connection2} = esputnik_api:send_alert(Server, Message1),
    {error, closed} = esputnik_api:send_alert(connection2, Message1),
    Config.

active_resolve(Config) ->
    Server = ?config(server, Config),
    Message = esputnik_api:to_sputnik_message(resolve, <<"dev">>, <<"active_resolve">>),
    {ok, <<"random">>, connection2} = esputnik_api:send_alert(Server, Message),
    Config.

alert_opts(Config) ->
    Server = ?config(server, Config),
    MessageId = <<"msg_id">>,
    Message = esputnik_api:to_sputnik_message(alert, <<"dev">>, <<"alert_opts">>,
                                              [{request_id, <<"test">>},
                                               {message_id, MessageId},
                                               {priority, critical}]),
    {ok, MessageId, connection2} = esputnik_api:send_alert(Server, Message),
    Config.

esputnik_server(Config) ->
    ok = esputnik:alert(alert, <<"team1">>, <<"esputnik_server1">>),
    <<"esputnik_server1">> = proplists:get_value(<<"team1">>, wait_for_message(1)),
    ok = esputnik:alert(alert, <<"team2">>, <<"esputnik_server2">>),
    <<"esputnik_server2">> = proplists:get_value(<<"team2">>, wait_for_message(2)),
    ok = esputnik:alert(alert, <<"team3">>, <<"esputnik_server3">>),
    <<"esputnik_server3">> = proplists:get_value(<<"team3">>, wait_for_message(3)),
    {changed, <<"test">>, <<"new_url">>} = esputnik:change_api_url(<<"new_url">>),
    ok = esputnik:alert(alert, <<"team4">>, <<"esputnik_server4">>),
    <<"esputnik_server4">> = proplists:get_value(<<"team4">>, wait_for_message(4)),
    Config.

sputnik_api_timeout(Config) ->
    application:set_env(esputnik, connect_timeout, 1),
    Message = esputnik_api:to_sputnik_message(alert, <<"dev">>, <<"timeout">>),
    {error, timeout} = esputnik_api:send_alert(<<"https://heroku.com">>, Message),
    Config.

sputnik_api_timeout_server(Config) ->
    ok = esputnik:alert(alert, <<"team">>, <<"esputnik_server">>),
    [{_, Msg, _}] = wait_for_event(),
    <<"at=handle_alert error=timeout", _/binary>> = list_to_binary(Msg),
    Config.

sputnik_server_throttle(Config) ->
    ok = esputnik:alert(alert, <<"team">>, <<"esputnik_server_throttle0">>),
    ok = esputnik:alert(alert, <<"team">>, <<"esputnik_server_throttle1">>),
    [{_, Msg, _}] = wait_for_event(),
    <<"at=handle_alert warning=throttled", _/binary>> = list_to_binary(Msg),
    Config.

no_api_set(Config) ->
    ok = esputnik:alert(alert, <<"team">>, <<"esputnik_server">>),
    [{_, Msg, _}] = wait_for_event(),
    <<"at=handle_alert warning=no_api_set", _/binary>> = list_to_binary(Msg),
    Config.

% Internal
wait_for_event() ->
    case gen_event:call(error_logger, esputnik_event_handler, get) of
        [] ->
            timer:sleep(1),
            wait_for_event();
        Events ->
            Events
    end.

wait_for_message(Length) ->
    case ets:tab2list(esputnik_server) of 
        List when length(List) == Length ->
            List;
        _ ->
            wait_for_message(Length)
    end.
