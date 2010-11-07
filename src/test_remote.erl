%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Remote test code
%% @copyright 2010 Alessandro Sivieri

-module(test_remote).
-export([main/0]).

get_function() ->
    F = fun(F) ->
        receive
            {Pid, [{"param", Num}]} ->
                {X, _} = string:to_integer(Num),
                Pid ! {self(), {"text/plain", integer_to_list(X*X)}},
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
    Res = httpc:request(post, {"https://localhost:8443/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_function(), [{"param", 4}])}, [crest_utils:ssl_options()], []),
    io:format("Answer: ~p~n", [Res]),
    halt(0).