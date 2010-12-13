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
%% @doc Load tests.
%% @copyright 2010 Alessandro Sivieri

-module(test_load).
-export([get_function_short/0, get_function_long/0]).

get_function_short() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Short computation"},
                F(F);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET"},
                F(F);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"input", "integer()"}]},
                F(F);
            {Pid, [{"input", InputString}]} ->
                spawn(fun() -> 
                    {InputInt, _} = string:to_integer(InputString),
                    Res = math:sinh(InputInt),
                    Pid ! {self(), {"text/plain", crest_utils:format("~f", [Res])}} end),
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

get_function_long() ->
    F = fun(F) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Long computation"},
                F(F);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET"},
                F(F);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"input", "string()"}]},
                F(F);
            {Pid, [{"input", InputString}]} ->
                spawn(fun() -> 
                    OutBin = lists:foldl(fun(_Elem, AccIn) ->
                                                    crypto:md5(AccIn)
                                                    end, InputString, lists:seq(1, 10)),
                    OutString = lists:flatten([io_lib:format("~2.16.0b",[N])||N<-binary_to_list(OutBin)]),
                    Pid ! {self(), {"text/plain", OutString}} end),
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.
