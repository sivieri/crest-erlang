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
%% @doc Remote test code
%% @copyright 2010 Alessandro Sivieri

-module(test_remote).
-export([main/0]).

get_function() ->
    F = fun(F) ->
        receive
            {Pid, [{"param", Num}]} ->
                {X, _} = string:to_integer(Num),
                Pid ! {self(), {"text/plain", integer_to_list(X*X)}},
                F(F);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

main() ->
    inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://localhost:8443/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(?MODULE, get_function(), [{"param", 4}])}, [crest_utils:ssl_options()], []),
    io:format("Answer: ~p~n", [Res]),
    halt(0).
