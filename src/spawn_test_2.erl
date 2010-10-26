%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Spawn test code
%% @copyright 2010 Alessandro Sivieri

-module(spawn_test_2).
-export([main/0]).

get_function() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Spawn test 2"},
                F(F);
            {Pid, _} ->
                Pid ! {self(), {"text/plain", "Function 2 called"}},
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

invocation(Max, Body) ->
	List = lists:seq(1, Max),
	crest_utils:pmap(fun(Index) ->
							 io:format("Spawning ~p~n", [Index]),
							 http:request("http://localhost:8001/crest/" ++ Body)
					 end, List).

main() ->
    inets:start(),
    % http:set_options([{proxy, {{"localhost", 8080}, []}}]),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_function())}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
            invocation(1000, Body);
        {ok, {_, Body}} ->
            invocation(1000, Body);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    halt(0).
