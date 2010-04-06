%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Main launcher.

-module(crest_exec).
-export([init/0, spawn_install/1, remote/1, spawn_exec/2]).

%% External API
init() ->
    register(crest, spawn(fun() -> loop([]) end)).

spawn_install(Params) ->
    F = binary_to_term(Params),
    rpc(crest, {install, F}).

spawn_exec([Key|T], Params) ->
    rpc(crest, {exec, Key, T, Params}).

remote(Params) ->
    io:format("~p~n", [Params]).

%% Internal API
loop(List) ->
    receive
        {Pid, {install, F}} ->
            {Key, Pid2} = crest_process:install(F),
            Pid ! {self(), Key},
            io:format("SPAWN: registered a key of value ~p~n", [Key]),
            loop([{Key, Pid2}|List]);
        {Pid, {exec, Key, T, Params}} ->
            case spawn_search(List, Key) of
                {ok, Pid2} ->
                    Res = rpc(Pid2, {T, Params}),
                    Pid ! {self(), {ok, Res}},
                    io:format("SPAWN: executed the key ~p~n", [Key]);
                {error} ->
                    Pid ! {self(), {error}}
            end,
            loop(List);
        {'EXIT', Pid, Reason} ->
            loop(List);
        Other ->
            io:format("SPAWN: ~p~n", [Other]),
            loop(List)
    end.

spawn_search([H|T], Index) ->
    case H of
        {Index, Pid} ->
            {ok, Pid};
        {_, _} ->
            spawn_search(T, Index)
    end;
spawn_search([], _) ->
    {error}.

rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {_, Response} ->
            Response
    end.
