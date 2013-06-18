-module(esputnik_event_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

init(_) ->
    {ok, []}.

handle_event({info_msg, _, Msg}, State) ->
    {ok, State++[Msg]};
handle_event(_, State) ->
    {ok, State}.

handle_call(get, State) ->
    {ok, State, []}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
