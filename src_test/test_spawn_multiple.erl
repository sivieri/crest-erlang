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
%% @doc Spawn test code, parallel invocations.
%% @copyright 2010 Alessandro Sivieri

-module(test_spawn_multiple).
-export([main/1, get_function_pow/0, get_function_sqrt/0, get_function_reverser/0, request_pow/1, request_sqrt/1, request_reverse/1]).

get_function_pow() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Power of integers"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"input", "integer()"}]},
                F(F);
            {Pid, [{"input", InputString}]} ->
				{InputInt, _} = string:to_integer(InputString),
				Res = math:pow(InputInt, 2),
                Pid ! {self(), {"text/plain", crest_utils:format("~f", [Res])}},
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

get_function_sqrt() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Square root of integers"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"input", "integer()"}]},
                F(F);
            {Pid, [{"input", InputString}]} ->
				{InputInt, _} = string:to_integer(InputString),
				Res = math:sqrt(InputInt),
                Pid ! {self(), {"text/plain", crest_utils:format("~f", [Res])}},
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

get_function_reverser() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "String reverser"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"input", "string()"}]},
                F(F);
            {Pid, [{"input", Input}]} ->
                Pid ! {self(), {"text/plain", lists:reverse(Input)}},
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

request_pow({Host, Key, _Index}) ->
	N = random:uniform(1000),
	N2 = crest_utils:format("~f", [math:pow(N, 2)]),
	case crest_operations:invoke_lambda(get, Host, Key, [{"input", crest_utils:format("~p", [N])}]) of
		{ok, Result} when N2 =:= Result->
			ok;
		{ok, Result} ->
			io:format("Wrong power result (~p, ~p)~n", [N, Result]);
		{error} ->
			io:format("Power error~n")
	end.

request_sqrt({Host, Key, _Index}) ->
	N = random:uniform(1000),
	N2 = crest_utils:format("~f", [math:sqrt(N)]),
	case crest_operations:invoke_lambda(get, Host, Key, [{"input", crest_utils:format("~p", [N])}]) of
		{ok, Result} when N2 =:= Result->
			ok;
		{ok, Result} ->
			io:format("Wrong square root result (~p, ~p)~n", [N, Result]);
		{error} ->
			io:format("Square root error~n")
	end.

request_reverse({Host, Key, _Index}) ->
	In = "Reversing strings...",
	Out = lists:reverse(In),
	case crest_operations:invoke_lambda(get, Host, Key, [{"input", In}]) of
		{ok, Result} when Out =:= Result->
			ok;
		{ok, Result} ->
			io:format("Wrong reverse result (~p, ~p)~n", [In, Result]);
		{error} ->
			io:format("Reverse error~n")
	end.

main([Host, SpawnsString, RequestsString]) ->
	{Spawns, _} = string:to_integer(SpawnsString),
	{Requests, _} = string:to_integer(RequestsString),
    List = lists:seq(1, Spawns),
	crest_utils:pmap(fun(_Index) ->
							 Res = crest_operations:invoke_spawn(Host, ?MODULE, get_function_pow),
    						 case Res of
        				         {ok, Body} ->
                                     List2 = lists:map(fun(Index) ->
															   {Host, Body, Index}
							  						   end, lists:seq(1, Requests)),
									 crest_utils:pmap(fun(Elem) -> test_spawn_multiple:request_pow(Elem) end, List2);
                                 {error} ->
                                     io:format("Spawn error~n")
                             end
					 end, List),
	crest_utils:pmap(fun(_Index) ->
							 Res = crest_operations:invoke_spawn(Host, ?MODULE, get_function_sqrt),
    						 case Res of
        				         {ok, Body} ->
                                     List2 = lists:map(fun(Index) ->
															   {Host, Body, Index}
							  						   end, lists:seq(1, Requests)),
									 crest_utils:pmap(fun(Elem) -> test_spawn_multiple:request_sqrt(Elem) end, List2);
                                 {error} ->
                                     io:format("Spawn error~n")
                             end
					 end, List),
	crest_utils:pmap(fun(_Index) ->
							 Res = crest_operations:invoke_spawn(Host, ?MODULE, get_function_reverser),
    						 case Res of
        				         {ok, Body} ->
                                     List2 = lists:map(fun(Index) ->
															   {Host, Body, Index}
							  						   end, lists:seq(1, Requests)),
									 crest_utils:pmap(fun(Elem) -> test_spawn_multiple:request_reverse(Elem) end, List2);
                                 {error} ->
                                     io:format("Spawn error~n")
                             end
					 end, List),
    halt(0).
