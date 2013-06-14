-module(esputnik_sup).
-behaviour(supervisor).
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    SputnikServer = esputnik_app:config(sputnik_api_url),
    {ok, {{one_for_one, 5, 10}, [{esputnik, {esputnik, start_link, [SputnikServer]}, permanent, 5000, worker, [esputnik]}]}}.
