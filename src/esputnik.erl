-module(esputnik).
-behaviour(gen_server).

-export([start_link/1]).

% API
-export([alert/3,
         alert/4]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {connection :: espuntik_api:connection()|undefined,
                server :: undefined|binary()}).

%% Public API
-spec start_link(esputnik_api:sputnik_path()) -> {ok, pid()}.
start_link(SputnikServer) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SputnikServer], []).

-spec alert(esputnik_api:alert_type(), esputnik_api:team_name(), esputnik_api:message()) ->
                   ok.
alert(AlertType, Team, Message) ->
    alert(AlertType, Team, Message, []).

-spec alert(esputnik_api:alert_type(), esputnik_api:team_name(), esputnik_api:message(),
            esputnik_api:alert_opts()) -> ok.
alert(AlertType, Team, Message, AlertOpts) ->
    SputnikMessage = esputnik_api:to_sputnik_message(AlertType, Team, Message, AlertOpts),
    gen_server:cast(?SERVER, {alert, SputnikMessage}).

%% Gen Server callbacks
init([SputnikServer]) ->
    {ok, #state{server=SputnikServer}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({alert, SputnikMessage}, #state{connection=undefined,
                                            server=Server}=State) ->
    {ok, _RequestId, Connection} = esputnik_api:send_alert(Server, SputnikMessage),
    {noreply, State#state{connection=Connection}};
handle_cast({alert, SputnikMessage}, #state{connection=Connection}=State) ->
    case esputnik_api:send_alert(Connection, SputnikMessage) of
        {error, closed} ->
            handle_cast({alert, SputnikMessage}, State#state{connection=undefined});
        {error, invalid_state} ->
            handle_cast({alert, SputnikMessage}, State#state{connection=undefined});
        {ok, _RequestId, Connection1} ->
            {noreply, State#state{connection=Connection1}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection=Connection}) ->
    esputnik_api:close_connection(Connection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
