%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc The demo.

-module(crest_demo).
-export([spawn_demo_1/0, spawn_demo_2/0, spawn_demo_3/0]).

%% External API
spawn_demo_1() ->
    inets:start(),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(get_word_frequency())}])}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
           Answer = get_header() ++
                "<form action=\"../crest/" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<label for=\"limit\">Lower limit for word frequency: </label><input type=\"text\" size=\"5\" maxlength=\"5\" name=\"limit\" value=\"10\" onKeyPress=\"return numbersonly(this, event)\" />" ++
                "<label for=\"addresses\">IP addresses of the local network computers (separated by newlines): </label><br/><textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer(),
           {ok, Answer};
        {ok, {_, Body}} ->
            Answer = get_header() ++
                "<p>Please, insert in the form below the addresses of the local network computers from which to gather the data; separate each address with a newline.</p>" ++
                "<form action=\"../crest/" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<input type=\"hidden\" name=\"limit\" value=\"10\"/>" ++
                "<textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer(),
            {ok, Answer};
        {error, Reason} ->
            Answer = get_header() ++
                "Error in spawning the demo: " ++ Reason ++ get_footer,
            {ok, Answer}
    end.

spawn_demo_2() ->
    inets:start(),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(get_inverse_document_frequency())}])}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
           Answer = get_header() ++
                "<form action=\"../crest/" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<label for=\"limit\">Lower limit for word frequency: </label><input type=\"text\" size=\"5\" maxlength=\"5\" name=\"limit\" value=\"10\" onKeyPress=\"return numbersonly(this, event)\" />" ++
                "<label for=\"addresses\">IP addresses of the local network computers (separated by newlines): </label><br/><textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer(),
           {ok, Answer};
        {ok, {_, Body}} ->
            Answer = get_header() ++
                "<p>Please, insert in the form below the addresses of the local network computers from which to gather the data; separate each address with a newline.</p>" ++
                "<form action=\"../crest/" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<input type=\"hidden\" name=\"limit\" value=\"10\"/>" ++
                "<textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer(),
            {ok, Answer};
        {error, Reason} ->
            Answer = get_header() ++
                "Error in spawning the demo: " ++ Reason ++ get_footer,
            {ok, Answer}
    end.

spawn_demo_3() ->
    inets:start(),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(get_cosine_similarity())}])}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
           Answer = get_header() ++
                "<form action=\"../crest/" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<label for=\"limit\">Lower limit for word frequency: </label><input type=\"text\" size=\"5\" maxlength=\"5\" name=\"limit\" value=\"10\" onKeyPress=\"return numbersonly(this, event)\" />" ++
                "<label for=\"addresses\">IP addresses of the local network computers (separated by newlines): </label><br/><textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer(),
           {ok, Answer};
        {ok, {_, Body}} ->
            Answer = get_header() ++
                "<p>Please, insert in the form below the addresses of the local network computers from which to gather the data; separate each address with a newline.</p>" ++
                "<form action=\"../crest/" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<input type=\"hidden\" name=\"limit\" value=\"10\"/>" ++
                "<textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer(),
            {ok, Answer};
        {error, Reason} ->
            Answer = get_header() ++
                "Error in spawning the demo: " ++ Reason ++ get_footer,
            {ok, Answer}
    end.

%% Internal API
get_header() ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"++
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" ++
    "<html xmlns=\"http://www.w3.org/1999/xhtml\">" ++
    "<head>" ++
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />" ++
    "<script src=\"../numbers.js\" type=\"text/javascript\"></script>" ++
    "<title>Computational REST - Erlang - Demo</title>" ++
    "</head>" ++
    "<body>" ++
    "<h1>Computational REST - Erlang - Demo</h1>".

get_footer() ->
    "<br/><p><a href=\"../demo.html\" title=\"Back to demo page\">Back to demo page</a></body></html>".

get_word_frequency() ->
    ClientFunction = fun() ->
            Words = fun(String) ->
                {match, Captures} = re:run(String, "\\b\\w+\\b", [global,{capture,first,list}]),
                [hd(C) || C<-Captures]
            end,
            ProcessEachLine = fun(ProcessEachLine, IoDevice, Dict) ->
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
                            Words(Data)),
                        ProcessEachLine(ProcessEachLine, IoDevice, NewDict)
                end
            end,
            GetWordCounts = fun(Filename) ->
                case file:open(Filename, read) of
                    {ok, IoDevice} ->
                        Dict = ProcessEachLine(ProcessEachLine, IoDevice, dict:new()),
                        Dict
                end
            end,
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = GetWordCounts(Filename),
                    Dict2 = dict:filter(fun(_Key, Value) -> if Value >= Limit -> true; Value < Limit -> false end end, Dict),
                    OrderedList = lists:sort(fun({_Word1, Count1}, {_Word2, Count2}) -> if Count1 =< Count2 -> true; Count1 > Count2 -> false end end, dict:to_list(Dict2)),
                    HtmlList = lists:foldl(fun({Count, Word}, AccIn) -> [lists:flatten(io_lib:format("<tr><td>~s</td><td>~p</td></tr>", [Word, Count]))|AccIn] end, [], OrderedList),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "<table>", HtmlList),
                    Pid ! {self(), {"text/html", Result ++ "</table>"}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~s", [Other]))}}
            end
        end,
    CalledFunction = fun({Address, Limit}, AccIn) ->
            Res = http:request(post, {"http://" ++ Address ++ ":8001/crest/remote", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(ClientFunction)}, {"filename", "/home/alex/demo.txt"}, {"limit", Limit}])}, [], []),
            case Res of
                {ok, {_, _, Body}} ->
                    [{Address, Body}|AccIn];
                {ok, {_, Body}} ->
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
                Pid ! {self(), {"text/html", get_header() ++ Result ++ get_footer()}},
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
            Words = fun(String) ->
                {match, Captures} = re:run(String, "\\b\\w+\\b", [global,{capture,first,list}]),
                [hd(C) || C<-Captures]
            end,
            ProcessEachLine = fun(ProcessEachLine, IoDevice, Dict) ->
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
                            Words(Data)),
                        ProcessEachLine(ProcessEachLine, IoDevice, NewDict)
                end
            end,
            GetWordCounts = fun(Filename) ->
                case file:open(Filename, read) of
                    {ok, IoDevice} ->
                        Dict = ProcessEachLine(ProcessEachLine, IoDevice, dict:new()),
                        Dict
                end
            end,
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = GetWordCounts(Filename),
                    Dict2 = dict:filter(fun(_Key, Value) -> if Value >= Limit -> true; Value < Limit -> false end end, Dict),
                    HtmlList = dict:fold(fun(Word, Count, AccIn) -> [lists:flatten(io_lib:format("<tr><td>~s</td><td>~p</td></tr>", [Word, Count]))|AccIn] end, [], Dict2),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "<table>", HtmlList),
                    Pid ! {self(), {"text/html", Result ++ "</table>"}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~s", [Other]))}}
            end
        end,
    CalledFunction = fun({Address, Limit}, AccIn) ->
            Res = http:request(post, {"http://" ++ Address ++ ":8001/crest/remote", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(ClientFunction)}, {"filename", "/home/alex/demo.txt"}, {"limit", Limit}])}, [], []),
            case Res of
                {ok, {_, _, Body}} ->
                    [{Address, Body}|AccIn];
                {ok, {_, Body}} ->
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
                Pid ! {self(), {"text/html", get_header() ++ Result ++ get_footer()}},
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
            Words = fun(String) ->
                {match, Captures} = re:run(String, "\\b\\w+\\b", [global,{capture,first,list}]),
                [hd(C) || C<-Captures]
            end,
            ProcessEachLine = fun(ProcessEachLine, IoDevice, Dict) ->
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
                            Words(Data)),
                        ProcessEachLine(ProcessEachLine, IoDevice, NewDict)
                end
            end,
            GetWordCounts = fun(Filename) ->
                case file:open(Filename, read) of
                    {ok, IoDevice} ->
                        Dict = ProcessEachLine(ProcessEachLine, IoDevice, dict:new()),
                        Dict
                end
            end,
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = GetWordCounts(Filename),
                    Dict2 = dict:filter(fun(_Key, Value) -> if Value >= Limit -> true; Value < Limit -> false end end, Dict),
                    HtmlList = dict:fold(fun(Word, Count, AccIn) -> [lists:flatten(io_lib:format("<tr><td>~s</td><td>~p</td></tr>", [Word, Count]))|AccIn] end, [], Dict2),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "<table>", HtmlList),
                    Pid ! {self(), {"text/html", Result ++ "</table>"}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~s", [Other]))}}
            end
        end,
    CalledFunction = fun({Address, Limit}, AccIn) ->
            Res = http:request(post, {"http://" ++ Address ++ ":8001/crest/remote", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(ClientFunction)}, {"filename", "/home/alex/demo.txt"}, {"limit", Limit}])}, [], []),
            case Res of
                {ok, {_, _, Body}} ->
                    [{Address, Body}|AccIn];
                {ok, {_, Body}} ->
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
                Pid ! {self(), {"text/html", get_header() ++ Result ++ get_footer()}},
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~s", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.
