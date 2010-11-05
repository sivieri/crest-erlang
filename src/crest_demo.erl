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
	ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_word_frequency())}, [], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
        {error, _Reason} ->
            {error}
    end.

spawn_demo_2() ->
    inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_inverse_document_frequency())}, [], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
        {error, _Reason} ->
            {error}
    end.

spawn_demo_3() ->
    inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_cosine_similarity())}, [], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
        {error, _Reason} ->
            {error}
    end.

get_word_frequency() ->
    ClientFunction = fun() ->
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}, {"address", Address}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = crest_wordlist:get_word_counts(Filename),
                    Dict2 = dict:filter(fun(_Key, Value) -> if Value >= Limit -> true; true -> false end end, Dict),
                    OrderedList = lists:sort(fun({_Word1, Count1}, {_Word2, Count2}) -> if Count1 =< Count2 -> true; Count1 > Count2 -> false end end, dict:to_list(Dict2)),
                    StructList = lists:map(fun({Word, Count}) -> {struct, [{erlang:iolist_to_binary("word"), erlang:iolist_to_binary(Word)}, {erlang:iolist_to_binary("frequency"), Count}]} end, OrderedList),
                    Result = {struct, [{erlang:iolist_to_binary("ip"), erlang:iolist_to_binary(Address)},
									   {erlang:iolist_to_binary("total"), length(StructList)},
									   {erlang:iolist_to_binary("words"), StructList}]},
                    Pid ! {self(), {"application/json", mochijson2:encode(Result)}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~p", [Other]))}}
            end
        end,
    CalledFunction = fun({Address, Limit}, AccIn) ->
            Res = httpc:request(post, {"https://" ++ Address ++ ":8443/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, ClientFunction, [{"filename", "/home/alex/demo.txt"}, {"limit", Limit}, {"address", Address}])}, [], []),
            case Res of
                {ok, {{_,200,_}, _, Body}} ->
                    [mochijson2:decode(Body)|AccIn];
                {error, Reason} ->
                    [Reason|AccIn]
            end
        end,
    F = fun(F) ->
        inets:start(),
		ssl:start(),
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Word frequency demo"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"addresses", "string()"}, {"limit", "integer()"}]},
                F(F);
            {Pid, [{"addresses", Addresses}, {"limit", Limit}]} ->
                AddressList = string:tokens(Addresses, "\r\n"),
                AddressList2 = lists:map(fun(Element) -> {Element, Limit} end, AddressList),
                Result = lists:foldl(CalledFunction, [], AddressList2),
                Pid ! {self(), {"application/json", mochijson2:encode(Result)}},
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

get_inverse_document_frequency() ->
	IDF = fun(Obj, N) ->
		% Recovering lists from JSON
		DictList = lists:map(fun(I) ->
									 Address = binary_to_list(crest_json:destructure(crest_utils:format("Obj[~p].ip", [I]), Obj)),
									 M = crest_json:destructure(crest_utils:format("Obj[~p].total", [I]), Obj),
									 Res = lists:map(fun(J) ->
													   Word = binary_to_list(crest_json:destructure(crest_utils:format("Obj[~p].words[~p].word", [I, J]), Obj)),
									 				   Frequency = crest_json:destructure(crest_utils:format("Obj[~p].words[~p].frequency", [I, J]), Obj),
													   {Word, Frequency}
													   end, lists:seq(0, M-1)),
									 {Address, dict:from_list(Res)}
									 end, lists:seq(0, N-1)),
		% IDF
        DictCount = lists:map(fun({_Address, Dict}) -> dict:map(fun(_Word, _Count) -> 1 end, Dict) end, DictList),
        MainDict = lists:foldl(fun(Dict, AccIn) -> dict:merge(fun(_Word, Count1, Count2) -> Count1 + Count2 end, Dict, AccIn) end, dict:new(), DictCount),
        FreqDict = dict:map(fun(_Word, Count) -> math:log(N / (1 + Count)) end, MainDict),
        Result = lists:map(fun({Address, Dict}) ->
                                         DescOrderedList = lists:sort(fun({_Word1, Count1}, {_Word2, Count2}) -> if Count1 >= Count2 -> true; Count1 < Count2 -> false end end, dict:to_list(Dict)),
                                         Folded = lists:foldl(fun({Word, Count}, AccIn) ->
                                            NewCount = Count * dict:fetch(Word, FreqDict),
                                            NewElement = {struct, [{erlang:iolist_to_binary("word"), erlang:iolist_to_binary(Word)}, {erlang:iolist_to_binary("frequency"), NewCount}]},
                                            [NewElement|AccIn]
                                         end, [], lists:sublist(DescOrderedList, 20)),
                                         {struct, [{erlang:iolist_to_binary("ip"), erlang:iolist_to_binary(Address)},{erlang:iolist_to_binary("words"), Folded}]}
                                         end, DictList),
        {self(), {"application/json", mochijson2:encode(Result)}}
		end,
	InvokeService = fun(Key, Addresses, Limit, N) ->
		Res2 = httpc:request(post, {"http://localhost:8080/crest/" ++ Key, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"addresses", Addresses}, {"limit", Limit}])}, [], []),
		case Res2 of
        	{ok, {{_,200,_}, _, Body2}} ->
            	Obj = mochijson2:decode(Body2),
				IDF(Obj, N);
        	{error, Reason2} ->
            	Reason2
    		end
		end,
    F = fun(F) ->
        inets:start(),
		ssl:start(),
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Inverse document frequency demo"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"addresses", "string()"}, {"limit", "integer()"}]},
                F(F);
            {Pid, [{"addresses", Addresses}, {"limit", Limit}]} ->
				AddressList = string:tokens(Addresses, "\r\n"),
        		DocumentNumber = length(AddressList),
				% Execution of the word frequency part
				Res = httpc:request("http://localhost:8080/demo?type=word"),
    			case Res of
        			{ok, {{_,200,_}, _, Body}} ->
            			Pid ! InvokeService(Body, Addresses, Limit, DocumentNumber);
        			{error, Reason} ->
            			Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Reason])}}
    			end,
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

get_cosine_similarity() ->
	Cosine = fun(Obj, N) ->
		% Recovering lists from JSON
		DictList = lists:map(fun(I) ->
									 Address = binary_to_list(crest_json:destructure(crest_utils:format("Obj[~p].ip", [I]), Obj)),
									 M = crest_json:destructure(crest_utils:format("Obj[~p].total", [I]), Obj),
									 Res = lists:map(fun(J) ->
													   Word = binary_to_list(crest_json:destructure(crest_utils:format("Obj[~p].words[~p].word", [I, J]), Obj)),
									 				   Frequency = crest_json:destructure(crest_utils:format("Obj[~p].words[~p].frequency", [I, J]), Obj),
													   {Word, Frequency}
													   end, lists:seq(0, M-1)),
									 {Address, dict:from_list(Res)}
									 end, lists:seq(0, N-1)),
		% Cosine
        Cosines = crest_cosine:cosine_documents(DictList),
        Result = lists:map(fun({Address1, Address2, Value}) ->
                                   {struct, [{erlang:iolist_to_binary("ip1"), erlang:iolist_to_binary(Address1)},
                                             {erlang:iolist_to_binary("ip2"), erlang:iolist_to_binary(Address2)},
                                             {erlang:iolist_to_binary("value"), Value}]}
                                   end, Cosines),
        {self(), {"application/json", mochijson2:encode(Result)}}
		end,
	InvokeService = fun(Key, Addresses, Limit, N) ->
		Res2 = httpc:request(post, {"http://localhost:8080/crest/" ++ Key, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"addresses", Addresses}, {"limit", Limit}])}, [], []),
		case Res2 of
        	{ok, {{_,200,_}, _, Body2}} ->
            	Obj = mochijson2:decode(Body2),
				Cosine(Obj, N);
        	{error, Reason2} ->
            	Reason2
    		end
		end,
    F = fun(F) ->
        inets:start(),
		ssl:start(),
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Cosine similarity demo"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"addresses", "string()"}, {"limit", "integer()"}]},
                F(F);
            {Pid, [{"addresses", Addresses}, {"limit", Limit}]} ->
				AddressList = string:tokens(Addresses, "\r\n"),
        		DocumentNumber = length(AddressList),
				% Execution of the word frequency part
				Res = httpc:request("http://localhost:8080/demo?type=word"),
    			case Res of
        			{ok, {{_,200,_}, _, Body}} ->
            			Pid ! InvokeService(Body, Addresses, Limit, DocumentNumber);
        			{error, Reason} ->
            			Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Reason])}}
    			end,
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.
