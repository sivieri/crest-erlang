%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Discovery service

-module(crest_discovery).
-export([init/0]).

%% External API
init() ->
    register(discovery, spawn(fun() -> loop([]) end)).

%% Internal API
loop(List) ->
    receive
        Other ->
            io:format("Wrong message: ~p~n", [Other]),
            loop(List)
    end.
