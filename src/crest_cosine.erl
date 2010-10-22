%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Utility functions for calculating the cosine similarity
%% @copyright 2010 Alessandro Sivieri

-module(crest_cosine).
-export([cosine_documents/1]).

%% External API

%% @doc This function takes a list of dictionaries, each of them
%% containing words with their frequency, and calculates the cosine
%% similarity between all pairs of dictionaries (that are documents).
%% @spec cosine_documents([{string(), dict()}]) -> [{string(), string(), float()}]
cosine_documents(ListOfDict) ->
    WordLists = lists:map(fun({_Address, SingleDict}) -> lists:sort(dict:fold(fun(Word, _Count, AccIn) -> [Word|AccIn] end, [], SingleDict)) end, ListOfDict),
    WordList = lists:umerge(WordLists),
    NormFreq = lists:map(fun({Address, SingleDict}) ->
                                 {Address, lists:map(fun(Word) -> find(Word, SingleDict) end, WordList)}
                                 end, ListOfDict),
    Max1 = lists:seq(1, length(NormFreq) - 1),
    Cosines = lists:foldl(fun(Index1, AccIn) ->
                          Max2 = lists:seq(Index1 + 1, length(NormFreq)),
                          Int = lists:map(fun(Index2) ->
                                                cosine_similarity(lists:nth(Index1, NormFreq), lists:nth(Index2, NormFreq))
                                                end, Max2),
                          [Int|AccIn]
                          end, [], Max1),
    lists:flatten(Cosines).

%% Internal API

cosine_similarity({Address1, List1}, {Address2, List2}) ->
    Dot = dot_product(List1, List2) / (magnitude(List1) * magnitude(List2)),
    {Address1, Address2, Dot}.

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
