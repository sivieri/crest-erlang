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
%% @doc Server module of a CREST peer.
%% It offers all the standard server start, stop and response
%% methods, plus the two specific CREST operations: spawn (split
%% into the installation and the execution parts) and remote.
%% All gen_server:call() operations are relegated to subprocesses,
%% so that the main server process is never blocked waiting for
%% an answer, and this allows this peer to call itself.
%% @copyright 2010 Alessandro Sivieri

-module(crest_peer).
-behaviour(gen_server).
-export([start/0, stop/0, spawn_install/1, remote/1, spawn_exec/2, add_child/2, remove_child/1, get_list/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% External API

%% @doc Start this peer
%% @spec start() -> ok
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop this peer
%% @spec stop() -> ok
stop() ->
    gen_server:call(?MODULE, stop).

%% @doc Install a new operation on this peer, using the given
%% parameters.
%% @spec spawn_install([{string(), any()}]) -> {string(), dictionary()}
spawn_install(Params) ->
    gen_server:call(?MODULE, {spawn, Params}).

%% @doc Execute an already installed operation, specified by the given
%% unique key.
%% @spec spawn_exec([Key], [{atom(), any()}]) -> {{ok, any()}, dictionary()} | {reply, {error}, dictionary()}
spawn_exec([Key], Params) ->
    gen_server:call(?MODULE, {exec, Key, Params});
spawn_exec(Key, Params) ->
    gen_server:call(?MODULE, {exec, Key, Params}).

%% @doc Install and execute a new operation on this peer, and then delete it.
%% @spec remote([{string(), any()}]) -> {{ok, any()}, dictionary()} | {{error}, dictionary()}
remote(Params) ->
    Key = gen_server:call(?MODULE, {spawn, lists:sublist(Params, 4)}),
    Answer = gen_server:call(?MODULE, {exec, Key, lists:sublist(Params, 5, length(Params))}),
    gen_server:cast(?MODULE, {delete, Key}),
    Answer.

%% @doc Add a new process to the internal server list, with the associated key.
%% @spec add_child(string(), pid()) -> dictionary()
add_child(Key, Pid) ->
    gen_server:cast(?MODULE, {add_child, Key, Pid}).

%% @doc Remove a child process from the internal server list, and terminate
%% it.
%% @spec remove_child(string()) -> dictionary()
remove_child(Key) ->
	gen_server:cast(?MODULE, {delete, Key}).

%% @doc Get a dictionary of responses from all childs, passing to all the given parameter;
%% the key is the child process UUID.
%% @spec get_list(string()) -> dictionary()
get_list(Param) ->
    gen_server:call(?MODULE, {list, {"param", Param}}).

init(_Args) ->
    Spawned = dict:new(),
	Children = supervisor:which_children(crest_spawn_sup),
	NewSpawned = lists:foldl(fun({Key, Pid, _, _}, AccIn) ->
									 log4erl:info("Recovered a running computation: ~p~n", [Key]),
									 dict:store(Key, Pid, AccIn) end,
							 Spawned, Children),
    {ok, NewSpawned}.

handle_call({spawn, Params}, From, Spawned) ->
	spawn(fun() -> handle_spawn(Params, From) end),
    {noreply, Spawned};
handle_call({exec, Key, Params}, From, Spawned) ->
	spawn(fun() -> handle_exec({Key, Params}, From, Spawned) end),
    {noreply, Spawned};
handle_call({list, Param}, From, Spawned) ->
	spawn(fun() -> handle_list(Param, From, Spawned) end),
    {noreply, Spawned};
handle_call(_Request, _From, Spawned) ->
    {noreply, Spawned}.

handle_cast({add_child, Key, Pid}, Spawned) ->
    NewSpawned = dict:store(Key, Pid, Spawned),
    log4erl:info("Registered a new key ~p~n", [Key]),
    {noreply, NewSpawned};
handle_cast({delete, Key}, Spawned) ->
    case supervisor:terminate_child(crest_sup, Key) of
        ok ->
            supervisor:delete_child(crest_sup, Key),
            NewSpawned = dict:erase(Key, Spawned),
            log4erl:info("Deleted the key ~p~n", [Key]),
            {noreply, NewSpawned};
        {error, not_found} ->
            {noreply, Spawned}
    end;
handle_cast(_Request, Spawned) ->
    {noreply, Spawned}.

handle_info(_Info, Spawned) ->
    {noreply, Spawned}.

code_change(_OldVsn, Spawned, _Extra) ->
    {ok, Spawned}.

terminate(_Reason, _Spawned) ->
    ok.

%% Internal API

handle_spawn(Params, From) ->
	F = crest_utils:get_lambda(Params),
    Key = crest_spawn:install(F),
	gen_server:reply(From, Key).

handle_exec({Key, Params}, From, Spawned) ->
	case dict:find(Key, Spawned) of
        {ok, ChildPid} ->
            Res = crest_utils:rpc(ChildPid, Params),
            log4erl:info("Executed the existing key ~p~n", [Key]),
			gen_server:reply(From, {ok, Res});
        error ->
			gen_server:reply(From, {error})
    end.

handle_list(Param, From, Spawned) ->
	Result = dict:fold(fun(Key, Pid, AccIn) ->
                               Val = {Key, crest_utils:rpc(Pid, Param)},
                               [Val|AccIn]
                               end, [], Spawned),
    log4erl:info("Collected all responses for parameter ~p~n", [Param]),
	gen_server:reply(From, dict:from_list(Result)).
