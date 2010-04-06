%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc TEMPLATE.

-module(crest).
-export([start/0, stop/0]).

%% External API
start() ->
    crest_deps:ensure(),
    ensure_started(crypto),
    application:start(crest).

stop() ->
    Res = application:stop(crest),
    application:stop(crypto),
    Res.

%% Internal API
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
