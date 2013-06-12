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
     alert_opts
    ].

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    ok = application:start(meck),
    application:set_env(esputnik, sputnik_api_endpoint, <<"test">>),
    Config.

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
                        {ok, 200, [], active_alert}
                end),
    meck:expect(hackney, body,
                fun(active_alert) ->
                        {ok, <<"{\"request_id\":\"random\"}">>, active_client}
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
                        {ok, 200, [], active_alert}
                end),
    meck:expect(hackney, body,
                fun(active_alert) ->
                        {ok, <<"{\"request_id\":\"random\"}">>, active_client}
                end),
    meck:expect(hackney, close,
                fun(active_client) ->
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
                                    fun(alert_opts) ->
                                            {ok, <<"{\"request_id\":\"", MessageId/binary  ,"\"}">>, active_client}
                                    end),
                        {ok, 200, [], alert_opts}
                end),
    meck:expect(hackney, close,
                fun(active_client) ->
                        ok
                end),
    Config;
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    meck:unload(hackney),
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
active_alert(Config) ->
    {ok, <<"random">>} = esputnik:alert(<<"dev">>, <<"active_alert">>, alert),
    Config.

active_resolve(Config) ->
    {ok, <<"random">>} = esputnik:alert(<<"dev">>, <<"active_resolve">>, resolve),
    Config.

alert_opts(Config) ->
    MessageId = <<"msg_id">>,
    {ok, MessageId} = esputnik:alert(<<"dev">>, <<"alert_opts">>, alert,
                                     [{request_id, <<"test">>},
                                      {message_id, MessageId},
                                      {priority, critical}]),
    Config.
