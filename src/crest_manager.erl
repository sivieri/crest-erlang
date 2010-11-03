%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Manager module: it invokes all the currently spawned processes,
%% searching for some parameters, then returns all data to the caller in
%% JSON format.
%% @copyright 2010 Alessandro Sivieri

-module(crest_manager).
-export([get_data/0]).

%% External API

%% @doc Function to get names, operations and parameters for all the
%% installed computations in this CREST peer.
%% @spec get_data() -> json()
get_data() ->
    NameDict = crest_peer:get_list({"param", "name"}),
	OperationDict = crest_peer:get_list({"param", "operation"}),
	ParamsDict = crest_peer:get_list({"param", "parameters"}),
	Temp1 = dict:merge(fun(_Key, Value1, Value2) -> {Value1, Value2} end, NameDict, OperationDict),
    Temp2 = dict:merge(fun(_Key, Value1, Value2) -> {Value1, compat(Value2)} end, Temp1, ParamsDict),
	ResultList = dict:fold(fun(Key, {{Name, Operation}, Params}, AccIn) ->
								   ElemList = [erlang:iolist_to_binary(make_link(Key)),
											   erlang:iolist_to_binary(Name),
											   erlang:iolist_to_binary(Operation),
											   erlang:iolist_to_binary(Params)],
								   [ElemList|AccIn]
								   end, [], Temp2),
	{struct, [{erlang:iolist_to_binary("aaData"), ResultList}]}.

%% Internal API

make_link(Value) ->
	"<a href=\"crest/" ++ Value ++ "\" title=\"" ++ Value ++ "\">" ++ Value ++ "</a>".

compat([]) ->
	"<em>None</em>";
compat(List) ->
	lists:foldl(fun({Name, Type}, AccIn) -> AccIn ++ Name ++ ": " ++ Type ++ "<br/>" end, "", List).
