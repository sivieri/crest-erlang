%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Main launcher.

-module(crest_exec).
-export([init/0, spawn_install/1, remote/1, spawn_exec/2]).

%% External API
init() ->
    register(crest, spawn(fun() -> loop([]) end)).

spawn_install(Params) ->
    {"code", Code} = first(Params),
    F = binary_to_term(list_to_binary(Code)),
    rpc(crest, {install, F}).

spawn_exec([Key], Params) ->
    rpc(crest, {exec, Key, Params});
spawn_exec(Key, Params) ->
    rpc(crest, {exec, Key, Params}).

remote([Param|T]) ->
    Key = spawn_install(Param),
    Answer = spawn_exec(Key, T),
    rpc(crest, {delete, Key}),
    Answer.

%% Internal API
loop(List) ->
    receive
        {Pid, {install, F}} ->
            {Key, Pid2} = crest_process:install(F),
            Pid ! {self(), Key},
            io:format("EXEC: registered a key of value ~p~n", [Key]),
            loop([{Key, Pid2}|List]);
        {Pid, {exec, Key, Params}} ->
            case spawn_search(List, Key) of
                {ok, Pid2} ->
                    Res = rpc(Pid2, Params),
                    Pid ! {self(), {ok, Res}},
                    io:format("EXEC: executed key ~p~n", [Key]);
                {error} ->
                    Pid ! {self(), {error}}
            end,
            loop(List);
        {Pid, {delete, Key}} ->
            DeleteFun = delete_fun(Key),
            NewList = lists:filter(DeleteFun, List),
            Pid ! {self(), ok},
            io:format("EXEC: deleted key ~p~n", [Key]),
            loop(NewList);
        {'EXIT', Pid, Reason} ->
            io:format("Pid ~p exited: ~p~n", [Pid, Reason]),
            loop(List);
        Other ->
            io:format("SPAWN: ~p~n", [Other]),
            loop(List)
    end.

delete_fun(Index) ->
    fun(Elem) ->
        case Elem of
            {Index, _} ->
                false;
            _ ->
                true
        end
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

first([First|_]) ->
    First;
first(First) ->
    First.

rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {_, Response} ->
            Response
    end.
