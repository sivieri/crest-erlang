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
%% @doc CREST local computations installer.
%% @copyright 2010 Alessandro Sivieri

-module(crest_local).
-behaviour(gen_server).
-export([start/0, stop/0, list_local/0, add_local/3, remove_local/1, start_local/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% External API
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

list_local() ->
	gen_server:call(?MODULE, list).

add_local(Name, Module, Function) ->
	gen_server:cast(?MODULE, {add, Name, Module, Function}).

remove_local(Name) ->
	gen_server:cast(?MODULE, {remove, Name}).

start_local(Name) ->
	gen_server:call(?MODULE, {start, Name}).

init(_Args) ->
    Locals = dict:new(),
    {ok, Locals}.

handle_call(list, _From, Locals) ->
	{reply, ok, Locals};
handle_call({start, Name}, _From, Locals) ->
	{reply, ok, Locals};
handle_call(_Request, _From, Locals) ->
    {noreply, Locals}.

handle_cast({add, Name, Module, Function}, Locals) ->
	{noreply, Locals};
handle_cast({remove, Name}, Locals) ->
	{noreply, locals};
handle_cast(_Request, Locals) ->
    {noreply, Locals}.

handle_info(_Info, Locals) ->
    {noreply, Locals}.

code_change(_OldVsn, Locals, _Extra) ->
    {ok, Locals}.

terminate(_Reason, _Locals) ->
    ok.

%% Internal API
