-module(esputnik).
-behaviour(gen_server).

-export([start_link/0]).

% API
-export([alert/3,
         alert/4,
         change_api_url/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {connection :: esputnik_api:connection()|undefined,
                server :: esputnik_api:sputnik_api_url(),
                last_message_timestamp :: erlang:timestamp()
               }).

%% Public API
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec alert(esputnik_api:alert_type(), esputnik_api:team_name(), esputnik_api:message()) ->
                   ok.
alert(AlertType, Team, Message) ->
    alert(AlertType, Team, Message, []).

-spec alert(esputnik_api:alert_type(), esputnik_api:team_name(), esputnik_api:message(),
            esputnik_api:alert_opts()) -> ok.
alert(AlertType, Team, Message, AlertOpts) ->
    SputnikMessage = esputnik_api:to_sputnik_message(AlertType, Team, Message, AlertOpts),
    gen_server:cast(?SERVER, {alert, SputnikMessage}).

-spec change_api_url(esputnik_api:sputnik_api_url()) -> {changed, esputnik_api:sputnik_api_url(),
                                                         esputnik_api:sputnik_api_url()}.
change_api_url(SputnikApiUrl) ->
    gen_server:call(?SERVER, {change_api_url, SputnikApiUrl}).

%% Gen Server callbacks
init([]) ->
    case esputnik_app:config(sputnik_api_url, undefined) of
        undefined ->
            error_logger:info_msg("at=init warning=no_api_set"),
            {stop, no_api_set};
        Url ->
            {ok, #state{server=Url}}
    end.

handle_call({change_api_url, SputnikServer}, _From, #state{connection=Connection,
                                                           server=OldSputnikServer}) ->
    case Connection of
        undefined ->
            ok;
        _ ->
            esputnik_api:close_connection(Connection)
    end,
    {reply, {changed, OldSputnikServer, SputnikServer}, #state{server=SputnikServer}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({alert, SputnikMessage}, #state{connection=undefined,
                                            last_message_timestamp=LastMessageTimestamp,
                                            server=Server}=State) ->
    case maybe_send_alert(Server, SputnikMessage, LastMessageTimestamp) of
        {error, reconnect} ->
            error_logger:info_msg("at=handle_alert warning=connection_closed message=~p", [SputnikMessage]),
            {noreply, State};
        {error, no_api_set} ->
            {noreply, State};
        throttled ->
            error_logger:info_msg("at=handle_alert warning=throttled message=~p", [SputnikMessage]),
            {noreply, State};
        {new_connection, Connection1} ->
            {noreply, State#state{connection=Connection1,
                                  last_message_timestamp=os:timestamp()}}
    end;
handle_cast({alert, SputnikMessage}, #state{connection=Connection,
                                            last_message_timestamp=LastMessageTimestamp}=State) ->
    case maybe_send_alert(Connection, SputnikMessage, LastMessageTimestamp) of
        {error, reconnect} ->
            handle_cast({alert, SputnikMessage}, State#state{connection=undefined});
        {error, no_api_set} ->
            {noreply, State};
        throttled ->
            error_logger:info_msg("at=handle_alert warning=throttled message=~p", [SputnikMessage]),
            {noreply, State};
        {new_connection, Connection1} ->
            {noreply, State#state{connection=Connection1,
                                  last_message_timestamp=os:timestamp()}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection=undefined}) ->
    ok;
terminate(_Reason, #state{connection=Connection}) ->
    esputnik_api:close_connection(Connection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
maybe_send_alert(undefined, _, _) ->
    {error, no_api_set};
maybe_send_alert(Connection, SputnikMessage, undefined) ->
    send_alert_(Connection, SputnikMessage);
maybe_send_alert(Connection, SputnikMessage, LastMessageTimestamp) ->
    ThrottleTime = esputnik_app:config(throttle_time),
    Diff = timer:now_diff(os:timestamp(), LastMessageTimestamp) / 1000,
    case Diff of
        X when X >= ThrottleTime ->
            send_alert_(Connection, SputnikMessage);
        _ ->
            throttled
    end.

send_alert_(Connection, SputnikMessage) ->
    case esputnik_api:send_alert(Connection, SputnikMessage) of
        {ok, _RequestId, Connection1} ->
            {new_connection, Connection1};
        {error, closed} ->
            {error, reconnect};
        {error, invalid_state} ->
            {error, reconnect};
        {error, connect_timeout} ->
            error_logger:info_msg("at=handle_alert error=timeout message=~p", [SputnikMessage]),
            esputnik_api:close_connection(Connection),
            {new_connection, undefined};
        {error, Error} ->
            error_logger:info_msg("at=handle_alert error=~p message=~p", [Error, SputnikMessage]),
            esputnik_api:close_connection(Connection),
            {new_connection, undefined}
    end.
