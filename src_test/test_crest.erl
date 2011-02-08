%% Copyright (c) 2011 Alessandro Sivieri <sivieri@elet.polimi.it>,
%% Gianpaolo Cugola <cugola@elet.polimi.it>
%% 
%% This file is part of CREST-Erlang.
%% 
%% CREST-Erlang is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% CREST-Erlang is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public License
%% along with CREST-Erlang. If not, see <http://www.gnu.org/licenses/>.
%% 
%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @author Gianpaolo Cugola <cugola@elet.polimi.it>
%% @doc Load tests.
%% @copyright 2011 Alessandro Sivieri

-module(test_crest).
-export([start/1, start_multiple/2, receiver/4, dumper/1, do_test/2]).
-define(NUM_ROUNDS, 240).
-define(INTERARRIVAL, 1).
-define(TEST_TYPE, "mochiweb").
-define(HOST, "192.168.1.3").
-define(CLIENT_SLEEP_TIME, 1000).

start(Filename) ->
    inets:start(),
    {ok, FileId} = file:open(Filename,[append]),
	{ok, DumpId} = file:open(Filename ++ ".dump", [write]),
    register(receiver, spawn(?MODULE, receiver, [0, 0, 0, FileId])),
	register(dumper, spawn(?MODULE, dumper, [DumpId])),
    do_round(0).

start_multiple(Filename, N) ->
	lists:foreach(fun(_) -> start(Filename) end, lists:seq(1, N)).

receiver(TotTime, TotBytes, NumReceived, OutFile) ->
    receive
	stop ->
	    io:fwrite("Stopping~n", []);
	{print, Time} when NumReceived>0 ->
	    io:fwrite("At ~w I got ~4b replies with an average respond time of ~.2f ms transporting ~.2f KBps~n", [Time, NumReceived, TotTime/(NumReceived*1000), TotBytes/(30*1024)]),
	    io:fwrite(OutFile, "~3b ~5b ~12.2f ~12.2f~n", [Time, NumReceived, TotTime/(NumReceived*1000), TotBytes/(30*1024)]),
	    receiver(0,0,0,OutFile);
	{print, Time} ->
	    io:fwrite("At ~w I got no replies~n", [Time]),
	    io:fwrite(OutFile, "~3b ~5b ~12.2f ~12.2f~n", [Time, 0, 0.0, 0.0]),
	    receiver(0,0,0,OutFile);
	{Time, Bytes} ->
	    receiver(TotTime+Time, TotBytes+Bytes, NumReceived+1,OutFile)
    end.

dumper(DumpFile) ->
	receive
		{dump, Head, Body} ->
			io:fwrite(DumpFile, "~p~n~p~n", [Head, Body]),
			dumper(DumpFile);
		stop ->
			io:fwrite("Dump end~n", [])
	end.

do_round(Round) ->
    if
	Round rem 30 == 0 ->
	    receiver ! {print, Round},
	    spawn_client(Round);
	Round rem ?INTERARRIVAL == 0 ->
	    spawn_client(Round);
	true ->
	    ok
    end,
    timer:sleep(1000),
    if
	Round < ?NUM_ROUNDS ->
	    do_round(Round+1);
	true ->
	    receiver ! stop,
        dumper ! stop,
	    timer:sleep(2000),
	    inets:stop()
    end.

spawn_client(ClientNum) ->
    Profile = list_to_atom("client_"++integer_to_list(ClientNum)),
    inets:start(httpc, [{profile, Profile}]),
    spawn(?MODULE, do_test, [Profile, ?NUM_ROUNDS-ClientNum]).

do_test(Profile, Time) ->
    timer:exit_after(timer:seconds(Time), elapsed_time),
	case ?TEST_TYPE of
		"mochiweb" ->
			Len1 = "0",
			Len2 = "0",
			Url1 = "http://" ++ ?HOST ++ ":8080/short?input=",
			Url2 = "http://" ++ ?HOST ++ ":8080/long?input=";
		"erlang" ->
			{ok, {{_,200,_}, Head1, Key1}} = httpc:request("http://" ++ ?HOST ++ ":8080/crest/url/8b36f797-ef74-4ff1-abcf-cc88a4ccf6f5?service=short&port=8444", Profile),
            {"content-length", Len1} = lists:keyfind("content-length",1,Head1),
			dumper ! {dump, Head1, Key1},
			{ok, {{_,200,_}, Head2, Key2}} = httpc:request("http://" ++ ?HOST ++ ":8080/crest/url/8b36f797-ef74-4ff1-abcf-cc88a4ccf6f5?service=long&port=8444", Profile),
            {"content-length", Len2} = lists:keyfind("content-length",1,Head2),
			dumper ! {dump, Head2, Key2},
			Url1 = "http://" ++ ?HOST ++ ":8081/crest/url/" ++ Key1 ++ "?input=",
			Url2 = "http://" ++ ?HOST ++ ":8081/crest/url/" ++ Key2 ++ "?input="
	end,
	Len = list_to_integer(Len1) + list_to_integer(Len2),
	Urls = [Url1, Url2],
    do_test(Profile, Len, Urls).
    
do_test(Profile, Len, Urls) ->
    ITime = now(),
	String = rstring(),
	LLen = lists:map(fun(X) ->
                     {ok, {{_,200,_}, Head, Body}} = httpc:request(X ++ String, Profile),
                     {"content-length", Len1} = lists:keyfind("content-length",1,Head),
					 dumper ! {dump, Head, Body},
                     list_to_integer(Len1)
                 end,
                 Urls),
    FTime = now(),
    ElapsedTime = timer:now_diff(FTime,ITime),
	TLen = lists:sum(LLen) + Len,
    receiver ! {ElapsedTime, TLen},
    timer:sleep(?CLIENT_SLEEP_TIME),
    do_test(Profile, Len, Urls).

rstring() ->
	StartList = lists:seq(97, 122),
	shuffle(StartList).

shuffle(List) ->
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.
