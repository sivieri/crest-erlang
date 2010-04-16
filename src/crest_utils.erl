%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Miscellaneous utilities.

-module(crest_utils).
-export([first/1, format/2]).

%% External API
first([First|_]) ->
    First;
first(First) ->
    First.

format(String, Elements) ->
    Pass = io_lib:format(String, Elements),
    lists:flatten(Pass).

%% Internal API

