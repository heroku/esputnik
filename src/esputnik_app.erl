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

%% Application callbacks
-export([start/2, stop/1]).

-export([config/0, config/1, config/2]).

%%%===================================================================
%%% Convenience Functions
%%%===================================================================

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
start(_StartType, _StartArgs) ->
    esputnik_sup:start_link().

stop(_State) ->
    ok.

