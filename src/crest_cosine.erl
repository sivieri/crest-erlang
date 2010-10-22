%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Utility functions for calculating the cosine similarity
%% @copyright 2010 Alessandro Sivieri

-module(crest_cosine).
-export([cosine_documents/1, cosine_similarity/2]).

%% External API

cosine_documents(ListOfDict) ->
    WordLists = lists:map(fun({_Address, SingleDict}) -> lists:sort(dict:fold(fun(Word, _Count, AccIn) -> [Word|AccIn] end, [], SingleDict)) end, ListOfDict),
    WordList = lists:umerge(WordLists),
    NormFreq = lists:map(fun({Address, SingleDict}) ->
                                 {Address, lists:map(fun(Word) -> find(Word, SingleDict) end, WordList)}
                                 end, ListOfDict),
    NormFreq.

cosine_similarity(List1, List2) ->
    dot_product(List1, List2) / (magnitude(List1) * magnitude(List2)).

%% Internal API

find(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            0
    end.

dot_product(List1, List2) ->
    lists:foldl(fun(Element, AccIn) -> Element + AccIn end, 0, lists:zipwith(fun(Element1, Element2) -> Element1 * Element2 end, List1, List2)).
    
magnitude(List) ->
    math:sqrt(lists:foldl(fun(Element, AccIn) -> AccIn + math:pow(Element, 2) end, 0, List)).
