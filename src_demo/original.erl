%% Copyright (c) 2010 Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
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
%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc The original demo module.
%% @copyright 2010 Alessandro Sivieri

-module(original).
-export([get_function/0, get_manager/0]).

%% External API

get_function() ->
    F = fun(F, Instances) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Original demo instances list"},
                F(F, Instances);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET/POST"},
                F(F, Instances);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"instance", "string()"}]},
                F(F, Instances);
            {Pid, [{"instance", "new"}]} ->
                Res = crest_operations:install_local("manager"),
                case Res of
                    {ok, Body} ->
                        NewInstances = [Body|Instances],
                        Pid ! {self(), {"text/plain", Body}},
                        F(F, NewInstances);
                    {error} ->
                        Pid ! {self(), {"text/plain", crest_utils:format("Error")}},
                        F(F, Instances)
                end;
            {Pid, []} ->
                case Instances of
                    [H|_] ->
                        Pid ! {self(), {"text/plain", H}};
                    [] ->
                        Res = crest_operations:install_local("manager"),
                        case Res of
                            {ok, Body} ->
                                NewInstances = [Body|Instances],
                                Pid ! {self(), {"text/plain", Body}},
                                F(F, NewInstances);
                            {error} ->
                                Pid ! {self(), {"text/plain", crest_utils:format("Error")}}
                        end
                end,
                F(F, Instances);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F, Instances)
        end
    end,
    fun() ->
        F(F, [])
    end.

get_manager() ->
    F = fun(F, Instances) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Original demo main manager"},
                F(F, Instances);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET/POST"},
                F(F, Instances);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), []},
                F(F, Instances);
            {Pid, []} ->
                F(F, Instances);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F, Instances)
        end
    end,
    fun() ->
        F(F, [])
    end.

%% Internal API

