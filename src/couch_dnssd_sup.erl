
-module(couch_dnssd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).%%
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    DNSSD = case couch_config:get("dnssd", "enable", "true") of
        "true" ->
            case couch_config:get("dnssd", "advertise", "true") of
                "true" ->
                    [?CHILD(couch_dnssd_advertise),
                     ?CHILD(couch_dnssd)];
                _ ->
                    [?CHILD(couch_dnssd)]
            end;
        _ ->
            []
    end,

    Services = [],

    {ok, { {one_for_one, 5, 10}, Services ++ DNSSD} }.

