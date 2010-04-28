%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Miscellaneous utilities.

-module(crest_utils).
-export([format/2, rpc/2, first/1, pmap/2, cosine_similarity/2, cosine_matrix/2, get_lambda_params/2, get_lambda_params/3, get_lambda/1]).

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

cosine_similarity(List1, List2) ->
    dot_product(List1, List2) / (magnitude(List1) * magnitude(List2)).

cosine_matrix([H|T], AccIn) ->
    CosineList = lists:map(fun(Element) -> cosine_similarity(H, Element) end, T),
    cosine_matrix(T, [CosineList|AccIn]);
cosine_matrix([], AccIn) ->
    lists:reverse(AccIn).

get_lambda_params(ModuleName, Fun) ->
    get_lambda_params(ModuleName, Fun, []).
get_lambda_params(ModuleName, Fun, OtherList) ->
    {_Name, ModuleBinary, Filename} = code:get_object_code(ModuleName),
    FunBinary = term_to_binary(Fun),
    mochiweb_util:encode(lists:append([{"module", ModuleName}, {"binary", ModuleBinary}, {"filename", Filename}, {"code", FunBinary}], OtherList)).

get_lambda([{"module", ModuleName}, {"binary", ModuleBinary}, {"filename", Filename}, {"code", FunBinary}]) ->
    code:load_binary(ModuleName, Filename, list_to_binary(ModuleBinary)),
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

dot_product(List1, List2) ->
    lists:foldl(fun(Element, AccIn) -> Element + AccIn end, 0, lists:zipwith(fun(Element1, Element2) -> Element1 * Element2 end, List1, List2)).
    
magnitude(List) ->
    math:sqrt(lists:foldl(fun(Element, AccIn) -> AccIn + math:pow(Element, 2) end, 0, List)).

