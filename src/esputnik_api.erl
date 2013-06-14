-module(esputnik_api).

-export([send_alert/3,
         send_alert/2,
         to_sputnik_message/4,
         to_sputnik_message/3,
         close_connection/1
        ]).

-export_type([team_name/0,
              message/0,
              alert_type/0,
              alert_opts/0,
              connection/0,
              sputnik_message/0,
              sputnik_server/0,
              sputnik_api_url/0
             ]).

-type team_name() :: iolist().
-type message() :: iolist().
-type alert_type() :: alert|resolve.
-type request_id() :: iolist().
-type priority() :: critical|warning|notice.
-type alert_opt() :: {request_id, iolist()}|{message_id, iolist()}|
                          {priority, priority()}.
-type alert_opts() :: [alert_opt()].
-type alert_error() :: internal_error|{code, pos_integer()}.
-type alert_output() :: {ok, request_id(), connection()}|
                         {error, alert_error(), connection()}.
-type sputnik_api_url() :: iolist().
-opaque connection() :: record()|undefined.
-type sputnik_server() :: sputnik_api_url()|connection().
-opaque sputnik_message() :: [{binary(), binary()}].
-type sputnik_connect_opt() :: {connect_timeout|recv_timeout, pos_integer()}.
-type sputnik_connect_opts() :: [sputnik_connect_opt()]|[].

-spec send_alert(sputnik_server(), sputnik_message(), sputnik_connect_opts()) -> alert_output().
send_alert(SputnikServer, SputnikMessage, SputnikConnectOpts) ->
    SputnikConnectOpts1 = get_connect_opts(SputnikConnectOpts),
    http_post(SputnikServer, <<"/alert">>, SputnikMessage, SputnikConnectOpts1).

-spec send_alert(sputnik_server(), sputnik_message()) -> alert_output().
send_alert(SputnikServer, SputnikMessage) ->
    send_alert(SputnikServer, SputnikMessage, []).

-spec close_connection(connection()|undefined) -> ok.
close_connection(Connection) ->
    catch hackney:close(Connection),
    ok.

-spec to_sputnik_message(alert_type(), team_name(), message(), alert_opts()) ->
                                sputnik_message().
to_sputnik_message(AlertType, Team, Message, AlertOpts) ->
    [{<<"team">>, to_bin(Team)},
     {<<"message">>, to_bin(Message)},
     {<<"type">>, convert_alert(AlertType)}] ++ get_opts(AlertOpts).
-spec to_sputnik_message(alert_type(), team_name(), message()) -> sputnik_message().
to_sputnik_message(AlertType, Team, Message) ->
    to_sputnik_message(AlertType, Team, Message, []).
    
%% Internal
get_opts(Opts) ->
    get_opts(Opts, []).

get_opts([], Res) ->
    Res;
get_opts([{request_id, RequestId}|Rest], Res) ->
    get_opts(Rest, Res++[{<<"request_id">>, to_bin(RequestId)}]);
get_opts([{message_id, MessageId}|Rest], Res) ->
    get_opts(Rest, Res++[{<<"message_id">>, to_bin(MessageId)}]);
get_opts([{priority, Priority}|Rest], Res) ->
    get_opts(Rest, Res++[{<<"priority">>, convert_priority(Priority)}]).

get_connect_opts(Opts) ->
    get_connect_opts(Opts, []).

get_connect_opts([], Res) ->
    set_connect_defaults([{connect_timeout, esputnik_app:config(connect_timeout)},
                          {recv_timeout, esputnik_app:config(recv_timeout)}], Res);
get_connect_opts([{connect_timeout, ConnectTimeout}|Opts], Res) ->
    get_connect_opts(Opts, Res++[{connect_timeout, ConnectTimeout}]);
get_connect_opts([{recv_timeout, RecvTimeout}|Opts], Res) ->
    get_connect_opts(Opts, Res++[{recv_timeout, RecvTimeout}]).    

set_connect_defaults(Res, []) ->
    Res;
set_connect_defaults(Res, [{Key, DefValue}|Defaults]) ->
    case proplists:get_value(Key, Res) of
        undefined ->
            set_connect_defaults(Res++[{Key, DefValue}], Defaults);
        _->
            set_connect_defaults(Res, Defaults)
    end.

convert_alert(alert) ->
    <<"alert">>;
convert_alert(resolve) ->
    <<"resolve">>.

convert_priority(critical) ->
    <<"critical">>;
convert_priority(warning) ->
    <<"warning">>;
convert_priority(notice) ->
    <<"notice">>.

http_post(SputnikApiUrl, Endpoint, FormData, SputnikConnectOpts) when is_list(SputnikApiUrl) ->
    http_post(to_bin(SputnikApiUrl), Endpoint, FormData, SputnikConnectOpts);
http_post(SputnikApiUrl, Endpoint, FormData, SputnikConnectOpts) when is_binary(SputnikApiUrl) ->
    HttpReply = hackney:request(post, <<SputnikApiUrl/binary, Endpoint/binary>>, [], {form, FormData},
                                SputnikConnectOpts),
    http_handle(HttpReply);
http_post(SputnikClient, Endpoint, FormData, _) ->
    case hackney:send_request(SputnikClient, {post, Endpoint, [], {form, FormData}}) of
        {error, closed} ->
            {error, closed};
        {error, invalid_state} ->
            close_connection(SputnikClient),
            {error, invalid_state};
        HttpReply ->
            http_handle(HttpReply)
    end.

http_handle({ok, 200, _, Client}) ->
    {ok, Body, Client1} = hackney:body(Client),
    Json = jsx:json_to_term(Body),
    {ok, proplists:get_value(<<"request_id">>, Json), Client1};
http_handle({ok, 500, _, Client}) ->
    {error, internal_error, Client};
http_handle({ok, Code, _, Client}) ->
    {error, {code, Code}, Client};
http_handle({error, Reason}) ->
    {error, Reason}.

to_bin(B) when is_binary(B) ->
    B;
to_bin(L) when is_list(L) ->
    list_to_binary(L).
