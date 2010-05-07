%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Miscellaneous utilities.

-module(crest_utils).
-export([format/2, rpc/2, first/1, pmap/2, get_lambda_params/2, get_lambda_params/3, get_lambda/1]).

%% External API
first(Params) ->
    case is_list(Params) of
        true ->
            hd(Params);
        false ->
            Params
    end.

format(String, Elements) ->
    Pass = io_lib:format(String, Elements),
    lists:flatten(Pass).

rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {_, Response} ->
            Response
    end.

pmap(F, L) -> 
    S = self(),
    Ref = erlang:make_ref(), 
    Pids = lists:map(fun(I) -> 
               spawn(fun() -> do_f(S, Ref, F, I) end)
           end, L),
    gather(Pids, Ref).

get_lambda_params(ModuleName, Fun) ->
    get_lambda_params(ModuleName, Fun, []).
get_lambda_params(ModuleName, Fun, OtherList) ->
    {_Name, ModuleBinary, Filename} = code:get_object_code(ModuleName),
    FunBinary = term_to_binary(Fun),
    mochiweb_util:urlencode(lists:append([{"module", ModuleName}, {"binary", ModuleBinary}, {"filename", Filename}, {"code", FunBinary}], OtherList)).

get_lambda([{"module", ModuleName}, {"binary", ModuleBinary}, {"filename", Filename}, {"code", FunBinary}]) ->
    code:load_binary(list_to_atom(ModuleName), Filename, list_to_binary(ModuleBinary)),
    binary_to_term(list_to_binary(FunBinary)).

%% Internal API
do_f(Parent, Ref, F, I) ->                      
    Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) ->
    receive
    {Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end;
gather([], _) ->
    [].
