%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Spawn test code
%% @copyright 2010 Alessandro Sivieri

-module(spawn_test_3).
-export([main/0]).

get_function() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Spawn test 3"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), []},
                F(F);
            {Pid, _} ->
                Pid ! {self(), {"text/plain", "Function 3 called"}},
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
	ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_function())}, [crest_utils:ssl_options()], []),
    case Res of
        {ok, {_, _, Body}} ->
            io:format("Invocazione 1: ~p~n", [httpc:request("http://localhost:8080/crest/" ++ Body)]),
            io:format("Invocazione 2: ~p~n", [httpc:request("http://localhost:8080/crest/" ++ Body)]);
        {ok, {_, Body}} ->
            io:format("Invocazione 1: ~p~n", [httpc:request("http://localhost:8080/crest/" ++ Body)]),
            io:format("Invocazione 2: ~p~n", [httpc:request("http://localhost:8080/crest/" ++ Body)]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    halt(0).
