%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Remote test code

-module(remote_test).
-export([main/0]).

get_function() ->
    fun() ->
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
                    HtmlList = dict:fold(fun(Word, Count, AccIn) -> [lists:flatten(io_lib:format("<tr><td>~s</td><td>~p</td></tr>", [Word, Count]))|AccIn] end, [], Dict),
                    Result = lists:foldl(fun(Element, AccIn) -> AccIn ++ Element end, "<table>", HtmlList),
                    Pid ! {self(), {"text/html", Result ++ "</table>"}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~s", [Other]))}}
            end
        end.

main() ->
    inets:start(),
    % http:set_options([{proxy, {{"localhost", 8080}, []}}]),
    Res = http:request(post, {"http://localhost:8001/crest/remote", [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"code", term_to_binary(get_function())}, {"filename", "/home/alex/Downloads/demo.txt"}, {"limit", "10"}])}, [], []),
    io:format("Answer: ~p~n", [Res]),
    halt(0).
