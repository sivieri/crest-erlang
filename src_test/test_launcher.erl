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

-module(test_launcher).
-export([start/1, receiver/4, do_test/2]).
-define(NUM_ROUNDS, 180).
-define(INTERARRIVAL, 3).
-define(TEST_TYPE, "erlang").
-define(HOST, "131.175.135.132").
-define(CLIENT_SLEEP_TIME, 1000).

start(Filename) ->
    inets:start(),
    {ok, FileId} = file:open(Filename,[append]),
    register(receiver, spawn(?MODULE, receiver, [0, 0, 0, FileId])),
    do_round(0).

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
	    timer:sleep(2000),
	    inets:stop()
    end.

spawn_client(ClientNum) ->
    Profile = list_to_atom("client_"++integer_to_list(ClientNum)),
    inets:start(httpc, [{profile, Profile}]),
    spawn(?MODULE, do_test, [Profile, ?NUM_ROUNDS-ClientNum]).

do_test(Profile, Time) ->
    timer:exit_after(timer:seconds(Time), elapsed_time),
    do_test(Profile).
    
do_test(Profile) ->
    ITime = now(),
    case ?TEST_TYPE of
        "scheme" ->
            % SCHEME PART
            {ok, {{_,200,_}, Head, Body}} = httpc:request("http://" ++ ?HOST ++ ":8081/widget/manager/maps", Profile),
            {"content-length", L} = lists:keyfind("content-length",1,Head),
            Len = list_to_integer(L),
            Tok = string:tokens(Body,"{}[]:, \""),
            Widgets = [X || X <- Tok , string:str(X,"/mailbox/")==1],
            Urls = ["/static/dojo/demo/demo.html","/static/dojo/demo/dijit/themes/soria/images/titleBarActive.png","/static/dojo/demo/dijit/themes/soria/images/buttonActive.png"]++Widgets,
            LLen = lists:map(fun(X) ->
    			     {ok, {{_,200,_}, Head1, _}} = httpc:request("http://" ++ ?HOST ++ ":8081" ++ X, Profile),
    			     {"content-length", Len1} = lists:keyfind("content-length",1,Head1),
    			     list_to_integer(Len1)
    		     end,
    		     Urls);
        "mochiweb" ->
            % MOCHIWEB PART
            {ok, {{_,200,_}, Head, Body}} = httpc:request("http://" ++ ?HOST ++ ":8080/manager/widget/manager/maps", Profile),
            {"content-length", L} = lists:keyfind("content-length",1,Head),
            Len = list_to_integer(L),
            Tok = string:tokens(Body,"{}[]:, \""),
            Widgets = [X || X <- Tok , string:str(X,"/mailbox/")==1],
            Urls = ["/demo.html","/dijit/themes/soria/images/titleBarActive.png","/dijit/themes/soria/images/buttonActive.png"]++Widgets,
            LLen = lists:map(fun(X) ->
                     {ok, {{_,200,_}, Head1, _}} = httpc:request("http://" ++ ?HOST ++ ":8080" ++ X, Profile),
                     {"content-length", Len1} = lists:keyfind("content-length",1,Head1),
                     list_to_integer(Len1)
                 end,
                 Urls);
        "erlang" ->
            % ERLANG PART
            {ok, {{_,200,_}, Head, Body}} = httpc:request("http://" ++ ?HOST ++ ":8080/crest/url/c2723f0f-c123-4d30-91e0-a147576e311e/widget/manager/maps", Profile),
            {"content-length", L} = lists:keyfind("content-length",1,Head),
            Len = list_to_integer(L),
            Tok = string:tokens(Body,"{}[]:, \""),
            Widgets = [X || X <- Tok , string:str(X,"/crest/url/")==1],
            Urls = ["/original/demo.html","/original/dijit/themes/soria/images/titleBarActive.png","/original/dijit/themes/soria/images/buttonActive.png"]++Widgets,
            LLen = lists:map(fun(X) ->
    			     {ok, {{_,200,_}, Head1, _}} = httpc:request("http://" ++ ?HOST ++ ":8080" ++ X, Profile),
    			     {"content-length", Len1} = lists:keyfind("content-length",1,Head1),
    			     list_to_integer(Len1)
    		     end,
    		     Urls)
    end,
    % COMMON PART
    FTime = now(),
    ElapsedTime = timer:now_diff(FTime,ITime),
    TotLen = lists:sum(LLen)+Len,
    receiver ! {ElapsedTime, TotLen},
    timer:sleep(?CLIENT_SLEEP_TIME),
    do_test(Profile).
