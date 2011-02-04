%% Copyright (c) 2010,2011 Alessandro Sivieri <sivieri@elet.polimi.it>
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
%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc CREST local computations installer.
%% @copyright 2010,2011 Alessandro Sivieri

-module(crest_local).
-behaviour(gen_server2).
-include("crest.hrl").
-export([start/0, stop/0, list_local/0, add_local/4, remove_local/1, start_local/1, reload/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% External API

%% @doc Start this peer
%% @spec start() -> ok
start() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop this peer
%% @spec stop() -> ok
stop() ->
    gen_server2:call(?MODULE, stop).

%% @doc Get a list of local computations available to installation.
%% @spec list_local() -> [{string(), {string(), string()}}]
list_local() ->
	gen_server2:call(?MODULE, list).

%% @doc Add a new local computation.
%% @spec add_local(string(), string(), string(), boolean()) -> ok
add_local(Name, Module, Function, Visibility) ->
    Computation = #computation{name = Name, module = Module, function = Function, visibility = Visibility},
	gen_server2:cast(?MODULE, {add, Computation}).

%% @doc Remove a computation from  the local ones; it replaces the current one.
%% @spec remove_local(string()) -> ok
remove_local(Name) ->
	gen_server2:cast(?MODULE, {remove, Name}).

%% @doc Install a computation locally, if present in the list.
%% @spec start_local(string()) -> {ok, Key} | error
start_local(Name) ->
	gen_server2:call(?MODULE, {start, Name}).

%% @doc Reload the configuration file; do not replace any function already
%% in the list.
%% @spec reload() -> ok
reload() ->
	gen_server2:cast(?MODULE, reload).

init(_Args) ->
    Locals = do_reload(),
    {ok, Locals}.

handle_call(list, _From, Locals) ->
    List = dict:fold(fun(Name, Computation, AccIn) ->
                             case Computation#computation.visibility of
                                 true ->
                                     [{Name, Computation#computation.module, Computation#computation.function}|AccIn];
                                 false ->
                                     AccIn
                             end
                             end, [], Locals),
	{reply, List, Locals};
handle_call({start, Name}, From, Locals) ->
	spawn(fun() -> handle_start(Name, From, Locals) end),
	{noreply, Locals};
handle_call(_Request, _From, Locals) ->
    {noreply, Locals}.

handle_cast({add, Computation}, Locals) ->
	log4erl:info("Registering a new local computation: ~p~n", [Computation#computation.name]),
	NewLocals = dict:store(Computation#computation.name, Computation, Locals),
	{noreply, NewLocals};
handle_cast({remove, Name}, Locals) ->
	log4erl:info("De-registering a local computation: ~p~n", [Name]),
	NewLocals = dict:erase(Name, Locals),
	{noreply, NewLocals};
handle_cast(reload, Locals) ->
	{noreply, do_reload(Locals)};
handle_cast(_Request, Locals) ->
    {noreply, Locals}.

handle_info(_Info, Locals) ->
    {noreply, Locals}.

code_change(_OldVsn, Locals, _Extra) ->
    {ok, Locals}.

terminate(_Reason, _Locals) ->
    ok.

%% Internal API

do_reload() ->
	do_reload(dict:new()).
do_reload(Locals) ->
	Filename = crest_deps:local_path(["config", "locals.config"]),
	case file:consult(Filename) of
		{ok, NewLocalsList} ->
			NewLocals = lists:foldl(fun(Computation, AccIn) ->
											dict:store(Computation#computation.name, Computation, AccIn)
											end, dict:new(), NewLocalsList),
			dict:merge(fun(_Key, Value1, _Value2) -> Value1 end, Locals, NewLocals);
		{error, Reason} ->
			log4erl:info("Unable to load locals configuration: ~p~n", [Reason]),
			Locals
	end.

do_start_local(Locals, Name) ->
	case dict:find(Name, Locals) of
		{ok, Computation} ->
			M = list_to_atom(Computation#computation.module),
			F = list_to_atom(Computation#computation.function),
            crest_operations:invoke_local_spawn(fun() -> M:F() end);
		error ->
			{error}
	end.

handle_start(Name, From, Locals) ->
	Result = do_start_local(Locals, Name),
	gen_server2:reply(From, Result).
