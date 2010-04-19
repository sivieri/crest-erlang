%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Callbacks for the crest application.

-module(crest_app).
-behaviour(application).
-export([start/2,stop/1]).

%% External API
start(_Type, _StartArgs) ->
    crest_deps:ensure(),
    crest_sup:start_link().

stop(_State) ->
    ok.
