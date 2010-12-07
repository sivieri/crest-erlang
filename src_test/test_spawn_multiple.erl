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
%% @doc Spawn test code
%% @copyright 2010 Alessandro Sivieri

-module(test_spawn_multiple).
-export([main/0, get_function/0]).

get_function() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Spawn test 2"},
                F(F);
			{Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET"},
                F(F);
			{Pid, {"param", "parameters"}} ->
                Pid ! {self(), []},
                F(F);
            {Pid, _} ->
                Pid ! {self(), {"text/plain", "Function 2 called"}},
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

invocation(Max, Body) ->
	List = lists:seq(1, Max),
	crest_utils:pmap(fun(Index) ->
							 io:format("Spawning ~p~n", [Index]),
							 crest_utils:invoke_lambda("localhost", Body, [])
					 end, List).

main() ->
    Res = crest_utils:invoke_spawn("localhost", ?MODULE, get_function),
    case Res of
        {ok, Body} ->
            invocation(1000, Body);
        error ->
            io:format("Error~n")
    end,
    halt(0).
