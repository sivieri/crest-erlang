%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc The demo module: it generates a few Web pages and calls a
%% CREST peer through HTTP.
%% @copyright 2010 Alessandro Sivieri

-module(crest_demo).
-compile(export_all).
-export([spawn_demo/1]).

%% External API

%% @doc Spawn the correct demo based on the parameter.
spawn_demo([{"type", "word"}]) ->
	spawn_demo_1();
spawn_demo([{"type", "idf"}]) ->
	spawn_demo_2();
spawn_demo([{"type", "cosine"}]) ->
	spawn_demo_3();
spawn_demo(_) ->
	{error}.

%% Internal API

spawn_demo_1() ->
    inets:start(),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_word_frequency())}, [], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
        {error, _Reason} ->
            {error}
    end.

spawn_demo_2() ->
    inets:start(),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_inverse_document_frequency())}, [], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
        {error, _Reason} ->
            {error}
    end.

spawn_demo_3() ->
    inets:start(),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_cosine_similarity())}, [], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
        {error, _Reason} ->
            {error}
    end.

get_word_frequency() ->
    ClientFunction = fun() ->
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = crest_wordlist:get_word_counts(Filename),
                    Dict2 = dict:filter(fun(_Key, Value) -> if Value >= Limit -> true; Value < Limit -> false end end, Dict),
                    OrderedList = lists:sort(fun({_Word1, Count1}, {_Word2, Count2}) -> if Count1 =< Count2 -> true; Count1 > Count2 -> false end end, dict:to_list(Dict2)),
                    HtmlList = lists:foldl(fun({Word, Count}, AccIn) -> [lists:flatten(io_lib:format("<tr><td>~s</td><td>~p</td></tr>", [Word, Count]))|AccIn] end, [], OrderedList),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "<table><tr><th>Word</th><th>Frequency</th></tr>", HtmlList),
                    Pid ! {self(), {"text/html", Result ++ "</table>"}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~s", [Other]))}}
            end
        end,
    CalledFunction = fun({Address, Limit}, AccIn) ->
            Res = http:request(post, {"http://" ++ Address ++ ":8001/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, ClientFunction, [{"filename", "/home/alex/demo.txt"}, {"limit", Limit}])}, [], []),
            case Res of
                {ok, {{_,200,_}, _, Body}} ->
                    [{Address, Body}|AccIn];
                {error, Reason} ->
                    [{Address, Reason}|AccIn]
            end
        end,
    F = fun(F) ->
        inets:start(),
        receive
            {Pid, [{"limit", Limit}, {"addresses", Addresses}, {"Submit", "Query"}]} ->
                AddressList = string:tokens(Addresses, "\r\n"),
                AddressList2 = lists:map(fun(Element) -> {Element, Limit} end, AddressList),
                Tables = lists:foldl(CalledFunction, [], AddressList2),
                Result = lists:foldr(fun({Address, Element}, AccIn) -> AccIn ++ lists:flatten(io_lib:format("<h1>~s</h1>", [Address])) ++ Element end, "", Tables),
                Pid ! {self(), {"text/html", Result}},
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~s", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

get_inverse_document_frequency() ->
    ClientFunction = fun() ->
            receive
                {Pid, [{"filename", Filename}]} ->
                    Dict = crest_wordlist:get_word_counts(Filename),
                    Total = dict:fold(fun(_Word, Count, AccIn) -> Count + AccIn end, 0, Dict),
                    Dict2 = dict:map(fun(_Word, Count) -> Count / Total end, Dict),
                    PlainList = dict:fold(fun(Word, Count, AccIn) -> [lists:flatten(io_lib:format("~s!~p$", [Word, Count]))|AccIn] end, [], Dict2),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "", PlainList),
                    Pid ! {self(), {"text/plain", Result}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~s", [Other]))}}
            end
        end,
    CalledFunction = fun(Address, AccIn) ->
            Res = http:request(post, {"http://" ++ Address ++ ":8001/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, ClientFunction, [{"filename", "/home/alex/demo.txt"}])}, [], []),
            case Res of
                {ok, {{_,200,_}, _, Body}} ->
                    [{Address, Body}|AccIn];
                {error, Reason} ->
                    [{Address, Reason}|AccIn]
            end
        end,
    F = fun(F) ->
        inets:start(),
        receive
            {Pid, [{"addresses", Addresses}, {"Submit", "Query"}]} ->
                AddressList = string:tokens(Addresses, "\r\n"),
                DocumentNumber = length(AddressList),
                Counts = lists:foldl(CalledFunction, [], AddressList),
                DictList = lists:map(fun({Address, SingleList}) ->
                                             Elements = string:tokens(SingleList, "$"),
                                             Lists = lists:map(fun(Element) -> case string:tokens(Element, "!") of [Word|[Count]] -> {Word, Count} end end, Elements),
                                             {Address, dict:from_list(Lists)}
                                             end, Counts),
                DictCount = lists:map(fun({_Address, Dict}) -> dict:map(fun(_Word, _Count) -> 1 end, Dict) end, DictList),
                MainDict = lists:foldl(fun(Dict, AccIn) -> dict:merge(fun(_Word, Count1, Count2) -> Count1 + Count2 end, Dict, AccIn) end, dict:new(), DictCount),
                FreqDict = dict:map(fun(_Word, Count) -> math:log(DocumentNumber / (1 + Count)) end, MainDict),
                Tables = lists:map(fun({Address, Dict}) ->
                                             {Address, dict:fold(fun(Word, Count, AccIn) ->
                                                               NewCount = list_to_float(Count) * dict:fetch(Word, FreqDict),
                                                               AccIn ++ lists:flatten(io_lib:format("<tr><td>~s</td><td>~p</td></tr>", [Word, NewCount]))
                                                               end, "<table><tr><th>Word</th><th>IDF</th></tr>", Dict) ++ "</table>"}
                                             end, DictList),
                Result = lists:foldr(fun({Address, Element}, AccIn) -> AccIn ++ lists:flatten(io_lib:format("<h1>~s</h1>", [Address])) ++ Element end, "", Tables),
                Pid ! {self(), {"text/html", Result}},
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~s", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

get_cosine_similarity() ->
    ClientFunction = fun() ->
            receive
                {Pid, [{"filename", Filename}]} ->
                    Dict = crest_wordlist:get_word_counts(Filename),
                    Total = dict:fold(fun(_Word, Count, AccIn) -> Count + AccIn end, 0, Dict),
                    Dict2 = dict:map(fun(_Word, Count) -> Count / Total end, Dict),
                    PlainList = dict:fold(fun(Word, Count, AccIn) -> [lists:flatten(io_lib:format("~s!~p$", [Word, Count]))|AccIn] end, [], Dict2),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "", PlainList),
                    Pid ! {self(), {"text/plain", Result}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~s", [Other]))}}
            end
        end,
    CalledFunction = fun(Address, AccIn) ->
            Res = http:request(post, {"http://" ++ Address ++ ":8001/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, ClientFunction, [{"filename", "/home/alex/demo.txt"}])}, [], []),
            case Res of
                {ok, {{_,200,_}, _, Body}} ->
                    [{Address, Body}|AccIn];
                {error, Reason} ->
                    [{Address, Reason}|AccIn]
            end
        end,
    F = fun(F) ->
        inets:start(),
        receive
            {Pid, [{"addresses", Addresses}, {"Submit", "Query"}]} ->
                AddressList = string:tokens(Addresses, "\r\n"),
                DocumentNumber = length(AddressList),
                Counts = lists:foldl(CalledFunction, [], AddressList),
                DictList = lists:map(fun({Address, SingleList}) ->
                                             Elements = string:tokens(SingleList, "$"),
                                             Lists = lists:map(fun(Element) -> case string:tokens(Element, "!") of [Word|[Count]] -> {Word, Count} end end, Elements),
                                             {Address, dict:from_list(Lists)}
                                             end, Counts),
                DictCount = lists:map(fun({_Address, Dict}) -> dict:map(fun(_Word, _Count) -> 1 end, Dict) end, DictList),
                MainDict = lists:foldl(fun(Dict, AccIn) -> dict:merge(fun(_Word, Count1, Count2) -> Count1 + Count2 end, Dict, AccIn) end, dict:new(), DictCount),
                FreqDict = dict:map(fun(_Word, Count) -> math:log(DocumentNumber / (1 + Count)) end, MainDict),
                IDFs = lists:map(fun({_Address, Dict}) ->
                                             dict:map(fun(Word, Count) ->
                                                               list_to_float(Count) * dict:fetch(Word, FreqDict)
                                                               end, Dict)
                                             end, DictList),
                CosIDFs = lists:map(fun(Dict) ->
                                             dict:fold(fun(_Word, IDF, AccIn) -> [IDF|AccIn] end, [], Dict)
                                             end, IDFs),
                Cosines = cosine_matrix(CosIDFs, []),
                Result = lists:foldl(fun(Element, AccIn) ->
                                             AccIn ++ lists:foldl(fun(Element2, AccIn2) -> AccIn2 ++ lists:flatten(io_lib:format("<td>~p</td>", [Element2])) end, "<tr>", Element) ++ "</tr>"
                                             end, "<table>", Cosines) ++ "</table>",
                Pid ! {self(), {"text/html", Result}},
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~s", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

cosine_similarity(List1, List2) ->
    dot_product(List1, List2) / (magnitude(List1) * magnitude(List2)).

cosine_matrix([H|T], AccIn) ->
    CosineList = lists:map(fun(Element) -> cosine_similarity(H, Element) end, T),
    cosine_matrix(T, [CosineList|AccIn]);
cosine_matrix([], AccIn) ->
    lists:reverse(AccIn).

dot_product(List1, List2) ->
    lists:foldl(fun(Element, AccIn) -> Element + AccIn end, 0, lists:zipwith(fun(Element1, Element2) -> Element1 * Element2 end, List1, List2)).
    
magnitude(List) ->
    math:sqrt(lists:foldl(fun(Element, AccIn) -> AccIn + math:pow(Element, 2) end, 0, List)).
