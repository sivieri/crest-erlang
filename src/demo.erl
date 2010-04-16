%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc The demo.

-module(demo).
-export([start_demo/0, spawn_demo/0]).

%% External API
start_demo() ->
    get_header() ++
        "<p>Press the button to start the demo...</p>" ++
        "<form action=\"demo2\" method=\"GET\">" ++
        "<input type=\"submit\" name=\"Submit\" value=\"Start the demo\"/>" ++
        "</form>" ++
        get_footer().

spawn_demo() ->
    inets:start(),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(get_function())}])}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
           Answer = get_header() ++
                "<p>Please, insert in the form below the addresses of the local network computers from which to gather the data; separate each address with a newline.</p>" ++
                "<form action=\"" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<input type=\"hidden\" name=\"limit\" value=\"10\"/>" ++
                "<textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer(),
           {ok, Answer};
        {ok, {_, Body}} ->
            Answer = get_header() ++
                "<p>Please, insert in the form below the addresses of the local network computers from which to gather the data; separate each address with a newline.</p>" ++
                "<form action=\"" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
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
    "<title>Computational REST - Erlang - Demo</title>" ++
    "</head>" ++
    "<body>" ++
    "<h1>Computational REST - Erlang - Demo</h1>".

get_footer() ->
    "</body></html>".

get_function() ->
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
    ClientFunction = fun() ->
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = GetWordCounts(Filename),
                    HtmlList = dict:fold(fun(Word, Count, AccIn) -> [lists:flatten(io_lib:format("<tr><td>~s</td><td>~s</td></tr>", [Word, Count]))|AccIn] end, [], Dict),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "<table>", HtmlList),
                    Pid ! {self(), {"text/html", Result ++ "</table>"}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~s", [Other]))}}
            end
        end,
    CalledFunction = fun(Address, AccIn) ->
            Res = http:request(post, {"http://" ++ Address ++ ":8001/crest/remote", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(ClientFunction)}, {"filename", "/home/alex/demo.txt"}, {"limit", integer_to_list(0)}])}, [], []),
            case Res of
                {ok, {_, _, Body}} ->
                    [Body|AccIn];
                {ok, {_, Body}} ->
                    [Body|AccIn];
                {error, Reason} ->
                    [Reason|AccIn]
            end
        end,
    F = fun(F) ->
        inets:start(),
        receive
            {Pid, [{"limit", Num}, {"addresses", Addresses}, {"Submit", "Query"}]} ->
                {X, _} = string:to_integer(Num),
                AddressList = string:tokens(Addresses, "\r\n"),
                Tables = lists:foldl(CalledFunction, [], AddressList),
                Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "", Tables),
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
