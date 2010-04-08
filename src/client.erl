-module(client).
-export([main/0]).

get_function() ->
    fun() ->
        receive
            {Pid, _} ->
                Pid ! {self(), {"text/plain", "Function called"}}
        end
    end.

main() ->
    inets:start(),
    http:set_options([{proxy, {{"localhost", 8080}, []}}]),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(get_function())}])}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
            io:format("Invocazione 1: ~p~n", [http:request("http://localhost:8001/crest/" ++ Body)]),
            io:format("Invocazione 2: ~p~n", [http:request("http://localhost:8001/crest/" ++ Body)]);
        {ok, {_, Body}} ->
            io:format("Invocazione 1: ~p~n", [http:request("http://localhost:8001/crest/" ++ Body)]),
            io:format("Invocazione 2: ~p~n", [http:request("http://localhost:8001/crest/" ++ Body)]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    halt(0).
