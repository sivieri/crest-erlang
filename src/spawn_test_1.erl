%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Spawn test code
%% @copyright 2010 Alessandro Sivieri

-module(spawn_test_1).
-export([main/0]).

get_function() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Spawn test 1"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), []},
                F(F);
            {Pid, _} ->
                Pid ! {self(), {"text/plain", "Function 1 called"}},
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

main() ->
    inets:start(),
    % httpc:set_options([{proxy, {{"localhost", 8080}, []}}]),
    Res = httpc:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_function())}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
            io:format("Invocazione 1: ~p~n", [httpc:request("http://localhost:8001/crest/" ++ Body)]),
            io:format("Invocazione 2: ~p~n", [httpc:request("http://localhost:8001/crest/" ++ Body)]);
        {ok, {_, Body}} ->
            io:format("Invocazione 1: ~p~n", [httpc:request("http://localhost:8001/crest/" ++ Body)]),
            io:format("Invocazione 2: ~p~n", [httpc:request("http://localhost:8001/crest/" ++ Body)]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    halt(0).
