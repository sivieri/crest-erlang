%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Utility functions for calculating the cosine similarity
%% @copyright 2010 Alessandro Sivieri

-module(crest_cosine).
-export([cosine_matrix/2]).

%% External API

cosine_matrix([H|T], AccIn) ->
    CosineList = lists:map(fun(Element) -> cosine_similarity(H, Element) end, T),
    cosine_matrix(T, [CosineList|AccIn]);
cosine_matrix([], AccIn) ->
    lists:reverse(AccIn).

%% Internal API

cosine_similarity(List1, List2) ->
    dot_product(List1, List2) / (magnitude(List1) * magnitude(List2)).

dot_product(List1, List2) ->
    lists:foldl(fun(Element, AccIn) -> Element + AccIn end, 0, lists:zipwith(fun(Element1, Element2) -> Element1 * Element2 end, List1, List2)).
    
magnitude(List) ->
    math:sqrt(lists:foldl(fun(Element, AccIn) -> AccIn + math:pow(Element, 2) end, 0, List)).
