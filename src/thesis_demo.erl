%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc The demo module: it generates a few Web pages and calls a
%% CREST peer through HTTP.
%% @copyright 2010 Alessandro Sivieri

-module(thesis_demo).
-export([spawn_demo/1]).

%% External API

%% @doc Spawn the correct demo based on the parameter.
spawn_demo([{"type", "word"}]) ->
	spawn_demo_word();
spawn_demo([{"type", "tfidf"}]) ->
	spawn_demo_tfidf();
spawn_demo([{"type", "cosine"}]) ->
	spawn_demo_cosine();
spawn_demo([{"type", "wordstatus"}]) ->
    spawn_demo_wordstatus();
spawn_demo(_) ->
	{error}.

%% Internal API

spawn_demo_word() ->
    inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_word_frequency())}, [crest_utils:ssl_options()], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

spawn_demo_tfidf() ->
    inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_inverse_document_frequency())}, [crest_utils:ssl_options()], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

spawn_demo_cosine() ->
    inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_cosine_similarity())}, [crest_utils:ssl_options()], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

spawn_demo_wordstatus() ->
    inets:start(),
    ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_word_status_frequency())}, [crest_utils:ssl_options()], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
        {ok, {{_,_,_}, _, _}} ->
            {error};
        {error, _Reason} ->
            {error}
    end.

get_word_frequency() ->
    ClientFunction = fun() ->
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}, {"address", Address}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = thesis_text_mining:get_word_counts(Filename),
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
    CalledFunction = fun({Address, Filename, Limit}, AccIn) ->
            Res = httpc:request(post, {"https://" ++ Address ++ ":8443/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, ClientFunction, [{"filename", Filename}, {"limit", Limit}, {"address", Address}])}, [crest_utils:ssl_options()], []),
            case Res of
                {ok, {{_,200,_}, _, Body}} ->
                    [mochijson2:decode(Body)|AccIn];
				{ok, {{_,N,Msg}, _, _}} ->
					[crest_utils:format("~p: ~p", [N, Msg])|AccIn];
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
                Pid ! {self(), [{"addresses", "string()"}, {"filename", "string()"}, {"limit", "integer()"}]},
                F(F);
            {Pid, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]} ->
                AddressList = string:tokens(Addresses, "\r\n"),
                AddressList2 = lists:map(fun(Element) -> {Element, Filename, Limit} end, AddressList),
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
		% TF-IDF
        ValueDict = thesis_text_mining:tf_idf(DictList),
		Result = lists:map(fun({Address, Dict}) ->
								   Values = dict:fold(fun(Word, TfIdf, AccIn) ->
															  [{struct, [{erlang:iolist_to_binary("word"), erlang:iolist_to_binary(Word)}, {erlang:iolist_to_binary("frequency"), TfIdf}]}|AccIn]
															  end, [], Dict),
								   {struct, [{erlang:iolist_to_binary("ip"), erlang:iolist_to_binary(Address)},
											 {erlang:iolist_to_binary("words"), Values}]}
								   end, ValueDict),
        {self(), {"application/json", mochijson2:encode(Result)}}
		end,
	InvokeService = fun(Key, Addresses, Filename, Limit, N) ->
		Res2 = httpc:request(post, {"http://localhost:8080/crest/" ++ Key, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}])}, [], []),
		case Res2 of
        	{ok, {{_,200,_}, _, Body2}} ->
            	Obj = mochijson2:decode(Body2),
				IDF(Obj, N);
			{ok, {{_,N,Msg}, _, _}} ->
				crest_utils:format("~p: ~p", [N, Msg]);
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
                Pid ! {self(), [{"addresses", "string()"}, {"filename", "string()"}, {"limit", "integer()"}]},
                F(F);
            {Pid, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]} ->
				AddressList = string:tokens(Addresses, "\r\n"),
        		DocumentNumber = length(AddressList),
				% Execution of the word frequency part
				Res = httpc:request("http://localhost:8080/demo?type=word"),
    			case Res of
        			{ok, {{_,200,_}, _, Body}} ->
            			Pid ! InvokeService(Body, Addresses, Filename, Limit, DocumentNumber);
					{ok, {{_,N,Msg}, _, _}} ->
						Pid ! {self(), {"text/plain", crest_utils:format("Error ~p: ~p", [N, Msg])}};
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
        Cosines = thesis_text_mining:cosine_documents(DictList),
        Result = lists:map(fun({Address1, Address2, Value}) ->
                                   {struct, [{erlang:iolist_to_binary("ip1"), erlang:iolist_to_binary(Address1)},
                                             {erlang:iolist_to_binary("ip2"), erlang:iolist_to_binary(Address2)},
                                             {erlang:iolist_to_binary("value"), Value}]}
                                   end, Cosines),
        {self(), {"application/json", mochijson2:encode(Result)}}
		end,
	InvokeService = fun(Key, Addresses, Filename, Limit, N) ->
		Res2 = httpc:request(post, {"http://localhost:8080/crest/" ++ Key, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}])}, [], []),
		case Res2 of
        	{ok, {{_,200,_}, _, Body2}} ->
            	Obj = mochijson2:decode(Body2),
				Cosine(Obj, N);
			{ok, {{_,N,Msg}, _, _}} ->
				crest_utils:format("~p: ~p", [N, Msg]);
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
                Pid ! {self(), [{"addresses", "string()"}, {"filename", "string()"}, {"limit", "integer()"}]},
                F(F);
            {Pid, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]} ->
				AddressList = string:tokens(Addresses, "\r\n"),
        		DocumentNumber = length(AddressList),
				% Execution of the word frequency part
				Res = httpc:request("http://localhost:8080/demo?type=word"),
    			case Res of
        			{ok, {{_,200,_}, _, Body}} ->
            			Pid ! InvokeService(Body, Addresses, Filename, Limit, DocumentNumber);
					{ok, {{_,N,Msg}, _, _}} ->
						Pid ! {self(), {"text/plain", crest_utils:format("Error ~p: ~p", [N, Msg])}};
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

get_word_status_frequency() ->
        WordStatus = fun(Obj, N, OldDict) ->
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
        % Elaborate new status: DictList contains only one element
        {_, NewDict} = hd(DictList),
        dict:merge(fun(_Word, Value1, Value2) -> Value1 + Value2 end, OldDict, NewDict)
        end,
    InvokeService = fun(Key, Addresses, Filename, Limit, N, Status) ->
        AddressList = string:tokens(Addresses, "\r\n"),
        FinalDict = lists:foldl(fun(Address, AccIn) ->
                              Res2 = httpc:request(post, {"http://localhost:8080/crest/" ++ Key, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode([{"addresses", Address}, {"filename", Filename}, {"limit", Limit}])}, [], []),
                              case Res2 of
                                  {ok, {{_,200,_}, _, Body2}} ->
                                      Obj = mochijson2:decode(Body2),
                                      WordStatus(Obj, N, AccIn);
                                  {ok, {{_,_,_}, _, _}} ->
                                      AccIn;
                                  {error, _} ->
                                      AccIn
                              end
                              end, Status, AddressList),
        StructList = lists:map(fun({Word, Count}) -> {struct, [{erlang:iolist_to_binary("word"), erlang:iolist_to_binary(Word)}, {erlang:iolist_to_binary("frequency"), Count}]} end, dict:to_list(FinalDict)),
        Result = {struct, [{erlang:iolist_to_binary("ip"), erlang:iolist_to_binary("Merged values")},
                           {erlang:iolist_to_binary("total"), length(StructList)},
                           {erlang:iolist_to_binary("words"), StructList}]},
        {{self(), {"application/json", mochijson2:encode(Result)}}, FinalDict}
        end,
    F = fun(F, Status) ->
        inets:start(),
        ssl:start(),
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Word frequency demo, with status"},
                F(F, Status);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                F(F, Status);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"addresses", "string()"}, {"filename", "string()"}, {"limit", "integer()"}]},
                F(F, Status);
            {Pid, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]} ->
                AddressList = string:tokens(Addresses, "\r\n"),
                DocumentNumber = length(AddressList),
                % Execution of the word frequency part
                Res = httpc:request("http://localhost:8080/demo?type=word"),
                case Res of
                    {ok, {{_,200,_}, _, Body}} ->
                        {ResMsg, NewStatus} = InvokeService(Body, Addresses, Filename, Limit, DocumentNumber, Status),
                        Pid ! ResMsg,
                        F(F, NewStatus);
                    {ok, {{_,N,Msg}, _, _}} ->
                        Pid ! {self(), {"text/plain", crest_utils:format("Error ~p: ~p", [N, Msg])}},
                        F(F, Status);
                    {error, Reason} ->
                        Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Reason])}},
                        F(F, Status)
                end;
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                F(F, Status)
        end
    end,
    fun() ->
        F(F, dict:new())
    end.
