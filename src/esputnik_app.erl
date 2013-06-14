%%%-------------------------------------------------------------------
%% @copyright Geoff Cant, Omar Yasin
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc {{name}} callback module.
%% @end
%%%-------------------------------------------------------------------
-module(esputnik_app).

-behaviour(application).

-define(APP, esputnik).
-define(SUP, esputnik_sup).

%% Application callbacks
-export([start/2, stop/1]).

-export([config/0, config/1, config/2, start/0, a_start/2]).

%%%===================================================================
%%% Convenience Functions
%%%===================================================================

start() ->
    a_start(?APP, permanent).

config(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(Key) ->
    case application:get_env(?APP, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.

config() ->
    application:get_all_env(?APP).


%% ===================================================================
%% Application callbacks
%% ===================================================================
-ifdef(debug).
start(_StartType, _StartArgs) ->
    [_|Path] = lists:reverse(os:cmd("pwd")),
    code:add_path(lists:reverse(Path)),
    ?SUP:start_link().
-else.
start(_StartType, _StartArgs) ->
    ?SUP:start_link().
-endif.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

a_start(App, Type) ->
    start_ok(App, Type, application:start(App, Type)).

start_ok(_App, _Type, ok) -> ok;
start_ok(_App, _Type, {error, {already_started, _App}}) -> ok;
start_ok(App, Type, {error, {not_started, Dep}}) ->
    ok = a_start(Dep, Type),
    a_start(App, Type);
start_ok(App, _Type, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
