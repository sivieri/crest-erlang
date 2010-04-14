%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc The defining demo.

-module(demo).
-export([spawn_demo/0]).

%% External API
spawn_demo() ->
    ok.

%% Internal API
get_function() ->
    ClientFunction = fun() ->
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = wordlist:get_word_counts(Filename),
                    HtmlList = dict:fold(fun(Word, Count, AccIn) -> [io_lib:format("<tr><td>~p</td><td>~p</td></tr>", [Word, Count])|AccIn] end, [], Dict),
                    Result = lists:foldl(fun(Element, AccIn) -> string:concat(AccIn, Element) end, "<table>", HtmlList),
                    Pid ! {self(), {"text/html", string:concat(Result, "</table>")}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", io_lib:format("Error: ~p", [Other])}}
            end
        end,
    F = fun(F) ->
        inets:start(),
        receive
            {Pid, [{"limit", Num}, {"addresses", Addresses}]} ->
                {X, _} = string:to_integer(Num),
                AddressList = regexp:split(Addresses, ":"),
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", io_lib:format("Error: ~p", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.
