%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc The defining demo.

-module(demo).
-export([spawn_demo/0]).

%% External API
spawn_demo() ->
    ok.

%% Internal API
get_function() ->
    ClientFunction = fun() ->
            ok
        end,
    F = fun(F) ->
        receive
            {Pid, [{"limit", Num}]} ->
                {X, _} = string:to_integer(Num),
                
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", io_lib:format("Error: ~p", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

format_html(Results) ->
    ok.
