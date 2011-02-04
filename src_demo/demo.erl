%% Copyright (c) 2010,2011 Alessandro Sivieri <sivieri@elet.polimi.it>
%% 
%% This file is part of CREST-Erlang.
%% 
%% CREST-Erlang is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% CREST-Erlang is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with CREST-Erlang. If not, see <http://www.gnu.org/licenses/>.
%% 
%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc The demo module: it generates a few Web pages and calls a
%% CREST peer through HTTP.
%% @copyright 2010,2011 Alessandro Sivieri

-module(demo).
-export([word_frequency/0, inverse_document_frequency/0, cosine_similarity/0, word_status_frequency/0, word_status_frequency/1, word_called_function/2, word_client_function/0]).

%% External API

word_frequency() ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Word frequency demo"},
                word_frequency();
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                word_frequency();
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"addresses", "string()"}, {"filename", "string()"}, {"limit", "integer()"}]},
                word_frequency();
            {Pid, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]} ->
                AddressList = string:tokens(Addresses, "\r\n"),
                AddressList2 = lists:map(fun(Element) -> {Element, Filename, Limit} end, AddressList),
                Result = lists:foldl(word_called_function, [], AddressList2),
                Pid ! {self(), {"application/json", mochijson2:encode(Result)}},
                word_frequency();
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                word_frequency()
        end.

inverse_document_frequency() ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Inverse document frequency demo"},
                inverse_document_frequency();
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                inverse_document_frequency();
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"addresses", "string()"}, {"filename", "string()"}, {"limit", "integer()"}]},
                inverse_document_frequency();
            {Pid, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]} ->
				AddressList = string:tokens(Addresses, "\r\n"),
        		DocumentNumber = length(AddressList),
				% Execution of the word frequency part
				Res = crest_operations:install_local("word"),
    			case Res of
        			{ok, Body} ->
            			Pid ! idf_invoke_service(Body, Addresses, Filename, Limit, DocumentNumber);
					{error} ->
            			Pid ! {self(), {"text/plain", crest_utils:format("Error")}}
    			end,
                inverse_document_frequency();
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                inverse_document_frequency()
        end.

cosine_similarity() ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Cosine similarity demo"},
                cosine_similarity();
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                cosine_similarity();
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"addresses", "string()"}, {"filename", "string()"}, {"limit", "integer()"}]},
                cosine_similarity();
            {Pid, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]} ->
				AddressList = string:tokens(Addresses, "\r\n"),
        		DocumentNumber = length(AddressList),
				% Execution of the word frequency part
				Res = crest_operations:install_local("word"),
    			case Res of
        			{ok, Body} ->
            			Pid ! cosine_invoke_service(Body, Addresses, Filename, Limit, DocumentNumber);
					{error} ->
            			Pid ! {self(), {"text/plain", crest_utils:format("Error")}}
    			end,
                cosine_similarity();
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                cosine_similarity()
        end.

word_status_frequency() ->
	word_status_frequency(dict:new()).

word_status_frequency(Status) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Word frequency demo, with status"},
                word_status_frequency(Status);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                word_status_frequency(Status);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"addresses", "string()"}, {"filename", "string()"}, {"limit", "integer()"}]},
                word_status_frequency(Status);
            {Pid, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]} ->
                AddressList = string:tokens(Addresses, "\r\n"),
                DocumentNumber = length(AddressList),
                % Execution of the word frequency part
				Res = crest_operations:install_local("word"),
                case Res of
                    {ok, Body} ->
                        {ResMsg, NewStatus} = wordstatus_invoke_service(Body, Addresses, Filename, Limit, DocumentNumber, Status),
                        Pid ! ResMsg,
                        word_status_frequency(NewStatus);
                    {error} ->
                        Pid ! {self(), {"text/plain", crest_utils:format("Error")}},
                        word_status_frequency(Status)
                end;
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                word_status_frequency(Status)
        end.

%% Internal API

word_client_function() ->
            receive
                {Pid, [{"filename", Filename}, {"limit", Num}, {"address", Address}]} ->
                    {Limit, _} = string:to_integer(Num),
                    Dict = demo_text_mining:get_word_counts(Filename),
                    Dict2 = dict:filter(fun(_Key, Value) when Value >= Limit -> true;
                                           (_Key, _Value) -> false end, Dict),
                    OrderedList = lists:sort(fun({_Word1, Count1}, {_Word2, Count2}) when Count1 =< Count2 -> true;
                                                ({_Word1, _Count1}, {_Word2, _Count2}) -> false end, dict:to_list(Dict2)),
                    StructList = lists:map(fun({Word, Count}) -> {struct, [{erlang:iolist_to_binary("word"), erlang:iolist_to_binary(Word)}, {erlang:iolist_to_binary("frequency"), Count}]} end, OrderedList),
                    Result = {struct, [{erlang:iolist_to_binary("ip"), erlang:iolist_to_binary(Address)},
									   {erlang:iolist_to_binary("total"), length(StructList)},
									   {erlang:iolist_to_binary("words"), StructList}]},
                    Pid ! {self(), {"application/json", mochijson2:encode(Result)}};
                {Pid, Other} ->
                    Pid ! {self(), {"text/plain", lists:flatten(io_lib:format("Error: ~p", [Other]))}}
            end.

word_called_function({Address, Filename, Limit}, AccIn) ->
			Res = crest_operations:invoke_remote(Address, ?MODULE, fun() -> word_client_function() end, [{"filename", Filename}, {"limit", Limit}, {"address", Address}]),
            case Res of
                {ok, Body} ->
                    [mochijson2:decode(Body)|AccIn];
                {error} ->
                    AccIn
            end.

idf(Obj, N) ->
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
        ValueDict = demo_text_mining:tf_idf(DictList),
		Result = lists:map(fun({Address, Dict}) ->
								   Values = dict:fold(fun(Word, TfIdf, AccIn) ->
															  [{struct, [{erlang:iolist_to_binary("word"), erlang:iolist_to_binary(Word)}, {erlang:iolist_to_binary("frequency"), TfIdf}]}|AccIn]
															  end, [], Dict),
								   {struct, [{erlang:iolist_to_binary("ip"), erlang:iolist_to_binary(Address)},
											 {erlang:iolist_to_binary("words"), Values}]}
								   end, ValueDict),
        {self(), {"application/json", mochijson2:encode(Result)}}.

idf_invoke_service(Key, Addresses, Filename, Limit, N) ->
		Res2 = crest_operations:invoke_lambda(post, "localhost", Key, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]),
		case Res2 of
        	{ok, Body2} ->
            	Obj = mochijson2:decode(Body2),
				idf(Obj, N);
			{error} ->
            	"error"
    		end.

cosine(Obj, N) ->
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
        Cosines = demo_text_mining:cosine_documents(DictList),
        Result = lists:map(fun({Address1, Address2, Value}) ->
                                   {struct, [{erlang:iolist_to_binary("ip1"), erlang:iolist_to_binary(Address1)},
                                             {erlang:iolist_to_binary("ip2"), erlang:iolist_to_binary(Address2)},
                                             {erlang:iolist_to_binary("value"), Value}]}
                                   end, Cosines),
        {self(), {"application/json", mochijson2:encode(Result)}}.

cosine_invoke_service(Key, Addresses, Filename, Limit, N) ->
		Res2 = crest_operations:invoke_lambda(post, "localhost", Key, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]),
		case Res2 of
        	{ok, Body2} ->
            	Obj = mochijson2:decode(Body2),
				cosine(Obj, N);
			{error} ->
            	"Error"
    		end.

wordstatus(Obj, N, OldDict) ->
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
        dict:merge(fun(_Word, Value1, Value2) -> Value1 + Value2 end, OldDict, NewDict).

wordstatus_invoke_service(Key, Addresses, Filename, Limit, N, Status) ->
        AddressList = string:tokens(Addresses, "\r\n"),
        case N of
            0 ->
                FinalDict = Status;
            _ ->
                FinalDict = lists:foldl(fun(Address, AccIn) ->
							  Res2 = crest_operations:invoke_lambda(post, "localhost", Key, [{"addresses", Address}, {"filename", Filename}, {"limit", Limit}]),
                              case Res2 of
                                  {ok, {{_,200,_}, _, Body2}} ->
                                      Obj = mochijson2:decode(Body2),
                                      wordstatus(Obj, N, AccIn);
                                  {ok, {{_,_,_}, _, _}} ->
                                      AccIn;
                                  {error, _} ->
                                      AccIn
                              end
                              end, Status, AddressList)
        end,
        StructList = lists:map(fun({Word, Count}) -> {struct, [{erlang:iolist_to_binary("word"), erlang:iolist_to_binary(Word)}, {erlang:iolist_to_binary("frequency"), Count}]} end, dict:to_list(FinalDict)),
        Result = {struct, [{erlang:iolist_to_binary("ip"), erlang:iolist_to_binary("Merged values")},
                           {erlang:iolist_to_binary("total"), length(StructList)},
                           {erlang:iolist_to_binary("words"), StructList}]},
        {{self(), {"application/json", mochijson2:encode([Result])}}, FinalDict}.
