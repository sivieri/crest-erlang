%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Main launcher.

-module(crest_exec).
-export([init/0, spawn_install/1, remote/1, spawn_exec/2]).

%% External API
init() ->
    register(crest, spawn(fun() -> loop([]) end)).

spawn_install(Params) ->
    {"code", Code} = crest_utils:first(Params),
    F = binary_to_term(list_to_binary(Code)),
    crest_utils:rpc(crest, {install, F}).

spawn_exec([Key], Params) ->
    crest_utils:rpc(crest, {exec, Key, Params});
spawn_exec(Key, Params) ->
    crest_utils:rpc(crest, {exec, Key, Params}).

remote([Param|T]) ->
    Key = spawn_install(Param),
    Answer = spawn_exec(Key, T),
    crest_utils:rpc(crest, {delete, Key}),
    Answer.

%% Internal API
loop(List) ->
    receive
        {Pid, {install, F}} ->
            {Key, Pid2} = crest_process:install(F),
            Pid ! {self(), Key},
            error_logger:info_msg("Registered a new key ~p~n", [Key]),
            loop([{Key, Pid2}|List]);
        {Pid, {exec, Key, Params}} ->
            case spawn_search(List, Key) of
                {ok, Pid2} ->
                    Res = crest_utils:rpc(Pid2, Params),
                    Pid ! {self(), {ok, Res}},
                    error_logger:info_msg("Executed the existing key ~p~n", [Key]);
                {error} ->
                    Pid ! {self(), {error}}
            end,
            loop(List);
        {Pid, {delete, Key}} ->
            DeleteFun = delete_fun(Key),
            NewList = lists:filter(DeleteFun, List),
            Pid ! {self(), ok},
            error_logger:info_msg("Deleted the key ~p~n", [Key]),
            loop(NewList);
        {'EXIT', Pid, Reason} ->
            error_logger:warning_msg("The spawned process ~p exited: ~p~n", [Pid, Reason]),
            loop(List);
        Other ->
            error_logger:warning_msg("Crest unknown parameter: ~p~n", [Other]),
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
