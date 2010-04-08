#!/usr/bin/env escript
-export([main/1]).

get_function() ->
    fun() ->
        receive
            {Pid, _} ->
                Pid ! {self(), {"text/plain", "Function called"}}
        end
    end.

main(_) ->
    inets:start(),
    http:set_options([{proxy, {{"localhost", 8080}, []}}]),
    Res = http:request(post, {"http://localhost:8000/crest/spawn", [], "application/x-www-form-urlencoded", string:concat("code=", term_to_binary(get_function()))}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
            io:format("Invocazione 1: ~p~n", [http:request("http://localhost:8000/crest/" ++ Body)]),
            io:format("Invocazione 2: ~p~n", [http:request("http://localhost:8000/crest/" ++ Body)]);
        {ok, {_, Body}} ->
            io:format("Invocazione 1: ~p~n", [http:request("http://localhost:8000/crest/" ++ Body)]),
            io:format("Invocazione 2: ~p~n", [http:request("http://localhost:8000/crest/" ++ Body)]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    halt(0).
