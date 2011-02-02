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
-export([get_word_frequency/0, get_inverse_document_frequency/0, get_cosine_similarity/0, get_word_status_frequency/0]).

%% External API

get_word_frequency() ->
    ClientFunction = fun() ->
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
            end
        end,
    CalledFunction = fun({Address, Filename, Limit}, AccIn) ->
			Res = crest_operations:invoke_remote(Address, ?MODULE, ClientFunction, [{"filename", Filename}, {"limit", Limit}, {"address", Address}]),
            case Res of
                {ok, Body} ->
                    [mochijson2:decode(Body)|AccIn];
                {error} ->
                    AccIn
            end
        end,
    F = fun(F) ->
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
        ValueDict = demo_text_mining:tf_idf(DictList),
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
		Res2 = crest_operations:invoke_lambda(post, "localhost", Key, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]),
		case Res2 of
        	{ok, Body2} ->
            	Obj = mochijson2:decode(Body2),
				IDF(Obj, N);
			{error} ->
            	"error"
    		end
		end,
    F = fun(F) ->
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
				Res = crest_operations:install_local("word"),
    			case Res of
        			{ok, Body} ->
            			Pid ! InvokeService(Body, Addresses, Filename, Limit, DocumentNumber);
					{error} ->
            			Pid ! {self(), {"text/plain", crest_utils:format("Error")}}
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
        Cosines = demo_text_mining:cosine_documents(DictList),
        Result = lists:map(fun({Address1, Address2, Value}) ->
                                   {struct, [{erlang:iolist_to_binary("ip1"), erlang:iolist_to_binary(Address1)},
                                             {erlang:iolist_to_binary("ip2"), erlang:iolist_to_binary(Address2)},
                                             {erlang:iolist_to_binary("value"), Value}]}
                                   end, Cosines),
        {self(), {"application/json", mochijson2:encode(Result)}}
		end,
	InvokeService = fun(Key, Addresses, Filename, Limit, N) ->
		Res2 = crest_operations:invoke_lambda(post, "localhost", Key, [{"addresses", Addresses}, {"filename", Filename}, {"limit", Limit}]),
		case Res2 of
        	{ok, Body2} ->
            	Obj = mochijson2:decode(Body2),
				Cosine(Obj, N);
			{error} ->
            	"Error"
    		end
		end,
    F = fun(F) ->
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
				Res = crest_operations:install_local("word"),
    			case Res of
        			{ok, Body} ->
            			Pid ! InvokeService(Body, Addresses, Filename, Limit, DocumentNumber);
					{error} ->
            			Pid ! {self(), {"text/plain", crest_utils:format("Error")}}
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
        case N of
            0 ->
                FinalDict = Status;
            _ ->
                FinalDict = lists:foldl(fun(Address, AccIn) ->
							  Res2 = crest_operations:invoke_lambda(post, "localhost", Key, [{"addresses", Address}, {"filename", Filename}, {"limit", Limit}]),
                              case Res2 of
                                  {ok, {{_,200,_}, _, Body2}} ->
                                      Obj = mochijson2:decode(Body2),
                                      WordStatus(Obj, N, AccIn);
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
        {{self(), {"application/json", mochijson2:encode([Result])}}, FinalDict}
        end,
    F = fun(F, Status) ->
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
				Res = crest_operations:install_local("word"),
                case Res of
                    {ok, Body} ->
                        {ResMsg, NewStatus} = InvokeService(Body, Addresses, Filename, Limit, DocumentNumber, Status),
                        Pid ! ResMsg,
                        F(F, NewStatus);
                    {error} ->
                        Pid ! {self(), {"text/plain", crest_utils:format("Error")}},
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

%% Internal API

