%% @author Someauthor <somemail@somedomain.xx>
%% @doc Sample module
%% @copyright yyyy Someauthor

-module(module_sample).
-export([get_function/0]).

%% External API

%% @doc Returns the closure to be executed remotely (as spawn or remote);
%% note that the function has to call itself tail-recursively, if it has
%% to be spawned (remote functions are invoked just once, and then their
%% process is ended).
%% It is required to correctly handle the param=name message, while the
%% other responses need to include the Content-Type parameter of the
%% answer data (see below).
%% How to send this closure to the CREST peer is left to the developer
%% (for example, through inets:http, or ibrowse).
%% @spec get_function() -> fun()
get_function() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Closure service name"},
                F(F);
            {Pid, [{Parameter1, Value1}, {Parameter2, Value2}]} ->
                computation(Value1, Value2),
                Pid ! {self(), {"text/plain", ComputationResults}},
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

%% Internal API
