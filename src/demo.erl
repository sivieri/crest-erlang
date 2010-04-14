%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc The demo.

-module(demo).
-export([start_demo/0, spawn_demo/0]).

%% External API
start_demo() ->
    get_header() ++
        "<p>Press the button to start the demo...</p>" ++
        "<form action=\"crest/demo2\" method=\"GET\">" ++
        "<input type=\"submit\" name=\"Submit\" value=\"Start the demo\"/>" ++
        "</form>" ++
        get_footer().

spawn_demo() ->
    inets:start(),
    Res = http:request(post, {"http://localhost:8001/crest/spawn", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(get_function())}])}, [], []),
    case Res of
        {ok, {_, _, Body}} ->
           get_header() ++
                "<p>Please, insert in the form below the addresses of the local network computers from which to gather the data; separate each address with a newline.</p>" ++
                "<form action=\"crest/" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer();
        {ok, {_, Body}} ->
            get_header() ++
                "<p>Please, insert in the form below the addresses of the local network computers from which to gather the data; separate each address with a newline.</p>" ++
                "<form action=\"crest/" ++ Body ++ "\" method=\"POST\" enctype=\"application/x-www-form-urlencoded\">" ++
                "<textarea name=\"addresses\" rows=\"5\" cols=\"60\"></textarea><br/>" ++
                "<input type=\"hidden\" name=\"limit\" value=\"10\"/>" ++
                "<input type=\"submit\" name=\"Submit\" value=\"Query\"/>" ++
                "</form>" ++
                get_footer();
        {error, Reason} ->
            get_header() ++
                "Error in spawning the demo: " ++ Reason ++ get_footer
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
    ClientFunction = fun() ->
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = wordlist:get_word_counts(Filename),
                    HtmlList = dict:fold(fun(Word, Count, AccIn) -> [io_lib:format("<tr><td>~p</td><td>~p</td></tr>", [Word, Count])|AccIn] end, [], Dict),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "<table>", HtmlList),
                    Pid ! {self(), {"text/html", Result ++ "</table>"}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", io_lib:format("Error: ~p", [Other])}}
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
                AddressList = re:split(Addresses, "\r", [{return, list}]),
                Tables = lists:foldl(CalledFunction, [], AddressList),
                Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "", Tables),
                Pid ! {self(), {"text/html", Result}},
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", io_lib:format("Error: ~p", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.
