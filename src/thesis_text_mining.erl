%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Utility functions for calculating text mining weights:
%% word frequency, term frequency - inverse document frequency,
%% cosine similarity.
%% This module is used for the thesis demo.
%% @reference Word frequency: http://www.roberthorvick.com/2009/07/02/word-frequency-redux-erlang-list-comprehension-regex-and-list-folding/
%% @reference Tf-Idf: http://en.wikipedia.org/wiki/Tf%E2%80%93idf
%% @copyright 2010 Alessandro Sivieri

-module(thesis_text_mining).
-export([cosine_documents/1, get_word_counts/1, print_dict/1, tf_idf/1]).

%% External API

%% @doc Get the word count as a dictionary from the specified file.
%% @spec get_word_counts(string()) -> dictionary()
get_word_counts(Filename) ->
    case file:open(Filename, read) of
        {ok, IoDevice} ->
            Dict = process_each_line(IoDevice, dict:new()),
            Dict
    end.

%% @doc Print the dictionary.
%% @spec print_dict(dictionary()) -> ok
print_dict(Dict) ->
    dict:fold(fun(Word, Count, AccIn) -> 
        io:format("~s: ~w~n", [Word, Count]), AccIn end, void, Dict).

tf_idf(DictList) ->
	TfDict = tf(DictList),
	IdfDict = idf(DictList),
	lists:map(fun({Address, Dict}) ->
					  NewDict = dict:map(fun(Word, Tf) -> Tf * dict:fetch(Word, IdfDict) end, Dict),
					  {Address, NewDict}
					  end, TfDict).

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

process_each_line(IoDevice, Dict) ->
    case io:get_line(IoDevice, "") of
        eof -> 
            file:close(IoDevice),
            Dict;
        {error, Reason} ->
            file:close(IoDevice),
            throw(Reason);
        Data ->
            NewDict = lists:foldl(
                        fun(W, D) -> dict:update(W, fun(C) -> C + 1 end, 1, D) end, 
                        Dict, 
                        words(string:to_lower(Data))),
            process_each_line(IoDevice, NewDict)
    end.

words(String) ->
    case re:run(String, "\\b\\w{4,}\\b", [global,{capture,first,list}]) of
        {match, Captures} ->
            [hd(C) || C<-Captures];
        nomatch ->
            []
    end.


tf(DictList) ->
	lists:map(fun({Address, Dict}) ->
					  Total = dict:fold(fun(_Word, Count, AccIn) -> Count + AccIn end, 0, Dict),
					  NewDict = dict:map(fun(_Word, Count) -> Count / Total end, Dict),
					  {Address, NewDict}
					  end, DictList).

idf(DictList) ->
	N = length(DictList),
	Wordlist = lists:foldl(fun(Dict, AccIn) ->
								   Keys = dict:fetch_keys(Dict),
								   OrderedKeys = lists:sort(Keys),
								   lists:umerge(OrderedKeys, AccIn)
								   end, [], DictList),
	Wordfreq = dict:from_list(lists:map(fun(Word) ->
								 Val = lists:foldl(fun({_Address, Dict}, AccIn) ->
														   case dict:is_key(Word, Dict) of
															   true ->
																   AccIn + 1;
															   false ->
																   AccIn
														   end
														   end, 0, DictList),
								 {Word, Val}
								 end, Wordlist)),
	dict:from_list(lists:map(fun(Word) ->
									 Val = math:log10(N / dict:fetch(Word, Wordfreq)),
									 {Word, Val}
									 end, Wordlist)).

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
