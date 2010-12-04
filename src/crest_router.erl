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
%% @doc CREST requests router.
%% @copyright 2010 Alessandro Sivieri

-module(crest_router).
-export([route/4]).

%% External API

%% @doc Routing method, it takes HTTP requests and routes them
%% to the correct peer server method.
%% @spec route('GET' | 'POST', [string()], {atom(), any()}, string()) -> {integer(), [{string(), any()}], [any()]}
route(Method, Params, ReqParams, ContentType) ->
	log4erl:info("Router: routing a ~p request for ~p~n", [Method, Params]),
    case Method of
        'GET' ->
            case Params of
                ["spawn"] ->
                    {405, [], []};
                ["remote"] ->
                    {405, [], []};
                T ->
                    case crest_peer:spawn_exec(T, ReqParams) of
                        {ok, {CT, Message}} ->
                            {200, [{"Content-Type", CT}], [Message]};
                        {error} ->
                            {404, [], []}
                    end
            end;
        'POST' ->
            case Params of
                ["spawn"] when ContentType =:= "application/x-www-form-urlencoded" ->
                    Key = crest_peer:spawn_install(ReqParams),
                    {200, [{"Content-Type", "text/plain"}], [Key]};
                ["remote"] when ContentType =:= "application/x-www-form-urlencoded" ->
                    case crest_peer:remote(ReqParams) of
                        {ok, {CT, Message}} ->
                            {200, [{"Content-Type", CT}], [Message]};
                        {error} ->
                            {404, [], []}
                    end;
                T ->
                    % POST request for a spawned app
                    case crest_peer:spawn_exec(T, ReqParams) of
                        {ok, {CT, Message}} ->
                            {200, [{"Content-Type", CT}], [Message]};
                        {error} ->
                            {404, [], []}
                    end
            end;
        _ ->
            {405, [], []}
    end.

%% Internal API
