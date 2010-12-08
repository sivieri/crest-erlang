%% Copyright (c) 2010 Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
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
%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Manager module: it invokes all the currently spawned processes,
%% searching for some parameters, then returns all data to the caller in
%% JSON format.
%% @copyright 2010 Alessandro Sivieri

-module(crest_manager).
-export([get_installed_data/0, get_local_data/0]).

%% External API

%% @doc Function to get names, operations and parameters for all the
%% installed computations in this CREST peer.
%% @spec get_installed_data() -> json()
get_installed_data() ->
    NameDict = crest_peer:get_list("name"),
	OperationDict = crest_peer:get_list("operation"),
	ParamsDict = crest_peer:get_list("parameters"),
	Temp1 = dict:merge(fun(_Key, Value1, Value2) -> {Value1, Value2} end, NameDict, OperationDict),
    Temp2 = dict:merge(fun(_Key, Value1, Value2) -> {Value1, compat(Value2)} end, Temp1, ParamsDict),
	ResultList = dict:fold(fun(Key, {{Name, Operation}, Params}, AccIn) ->
								   ElemList = [erlang:iolist_to_binary(make_url_link(Key)),
											   erlang:iolist_to_binary(Name),
											   erlang:iolist_to_binary(Operation),
											   erlang:iolist_to_binary(Params)],
								   [ElemList|AccIn]
								   end, [], Temp2),
	{struct, [{erlang:iolist_to_binary("aaData"), ResultList}]}.

%% @doc Function to get names, operations and parameters for all the
%% local computations in this CREST peer.
%% @spec get_local_data() -> json()
get_local_data() ->
    LocalList = crest_local:list_local(),
	ResultList = lists:foldl(fun({Name, {Module, Function}}, AccIn) ->
								   ElemList = [erlang:iolist_to_binary(make_local_link(Name)),
											   erlang:iolist_to_binary(Module ++ ":" ++ Function ++ "()")],
								   [ElemList|AccIn]
								   end, [], LocalList),
	{struct, [{erlang:iolist_to_binary("aaData"), ResultList}]}.

%% Internal API

make_url_link(Value) ->
	"<a href=\"crest/url/" ++ Value ++ "\" title=\"" ++ Value ++ "\">" ++ Value ++ "</a>".

make_local_link(Value) ->
	"<a href=\"crest/local/" ++ Value ++ "\" title=\"" ++ Value ++ "\">" ++ Value ++ "</a>".

compat([]) ->
	"<em>None</em>";
compat(List) ->
	lists:foldl(fun({Name, Type}, AccIn) -> AccIn ++ Name ++ ": " ++ Type ++ "<br/>" end, "", List).
