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
%% @doc Main Computational REST operations.
%% @copyright 2010 Alessandro Sivieri

-module(crest_operations).
-export([invoke_spawn/3, invoke_remote/4, invoke_lambda/3]).

%% External API

%% @doc Spawn a function on a certain host; the function module needs to be
%% in the Erlang path.
%% @spec invoke_spawn(string(), atom(), atom()) -> {ok, Key} | {error}
invoke_spawn(Host, Module, Function) ->
	inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://" ++ Host ++ ":8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(Module, Module:Function(), [])}, [crest_utils:ssl_options()], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

%% @doc Spawn a function on a certain host and invoke it with parameters;
%% the function module needs to be in the Erlang path.
%% @spec invoke_remote(string(), atom(), atom(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_remote(Host, Module, Function, Params) ->
	inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://" ++ Host ++ ":8443/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(Module, Module:Function(), Params)}, [crest_utils:ssl_options()], []),
	io:format("~p~n", [Res]),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

%% @doc Invoke an already installed computation with parameters.
%% @spec invoke_lambda(string(), string(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_lambda(Host, Key, Params) ->
	inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"http://"++ Host ++ ":8080/crest/url/" ++ Key, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode(Params)}, [], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

%% Internal API

