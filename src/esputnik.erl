-module(esputnik).

-export([alert/3,
         alert/4
        ]).

-export_type([team_name/0,
              message/0,
              alert_type/0,
              alert_opts/0
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

-spec alert(team_name(), message(), alert_type()) ->
                   {ok, request_id()}|{error, alert_error()}.
alert(Team, Message, AlertType) ->
    alert(Team, Message, AlertType, []).

-spec alert(team_name(), message(), alert_type(), alert_opts()) ->
                   {ok, request_id()}|{error, alert_error()}.
alert(Team, Message, AlertType, AlertOpts) ->
    SputnikApi = esputnik_app:config(sputnik_api_endpoint),
    FormData = [{<<"team">>, to_bin(Team)},
                {<<"message">>, to_bin(Message)},
                {<<"type">>, convert_alert(AlertType)}] ++ get_opts(AlertOpts),
    http_post(<<SputnikApi/binary, "/", "alert">>, FormData).

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

to_bin(B) when is_binary(B) ->
    B;
to_bin(L) when is_list(L) ->
    list_to_binary(L).

http_post(Endpoint, FormData) ->
    case hackney:request(post, Endpoint, [], {form, FormData}) of
        {ok, 200, _, Client} ->
            {ok, Body, Client1} = hackney:body(Client),
            Json = jsx:json_to_term(Body),
            hackney:close(Client1),
            {ok, proplists:get_value(<<"request_id">>, Json)};
        {ok, 500, _, Client} ->
            hackney:close(Client),
            {error, internal_error};
        {ok, Code, _, Client} ->
            hackney:close(Client),
            {error, {code, Code}};
        {error, Reason} ->
            {error, Reason}
    end.
