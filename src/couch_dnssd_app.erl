-module(couch_dnssd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Deps} = application:get_key(couch_dnssd, applications),
    true = lists:all(fun ensure_started/1, Deps),
    couch_dnssd_sup:start_link().

stop(_State) ->
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            true;
        {error, {already_started, App}} ->
            true;
        Else ->
            error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
            Else
    end.
