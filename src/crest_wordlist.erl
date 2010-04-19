%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc A module for obtaining a wordlist
%%      Code adapted from http://www.roberthorvick.com/2009/07/02/word-frequency-redux-erlang-list-comprehension-regex-and-list-folding/

-module(crest_wordlist).
-export([get_word_counts/1, print_dict/1]).

%% External API
get_word_counts(Filename) ->
    case file:open(Filename, read) of
        {ok, IoDevice} ->
            Dict = process_each_line(IoDevice, dict:new()),
            Dict
    end.

print_dict(Dict) ->
    dict:fold(fun(Word, Count, AccIn) -> 
        io:format("~s: ~w~n", [Word, Count]), AccIn end, void, Dict).

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
                        words(Data)),
            process_each_line(IoDevice, NewDict)
    end.

words(String) ->
    {match, Captures} = re:run(String, "\\b\\w+\\b", [global,{capture,first,list}]),
    [hd(C) || C<-Captures].
