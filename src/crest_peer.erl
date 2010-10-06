%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Server module of a CREST peer.
%% It offers all the standard server start, stop and response
%% methods, plus the two specific CREST operations: spawn (split
%% into the installation and the execution parts) and remote.
%% @copyright 2010 Alessandro Sivieri

-module(crest_peer).
-behaviour(gen_server).
-export([start/0, stop/0, spawn_install/1, remote/1, spawn_exec/2, add_child/2]).
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
%% @spec spawn_install([{string(), any()}]) -> {reply, string(), dictionary()}
spawn_install(Params) ->
    gen_server:call(?MODULE, {spawn, Params}).

%% @doc Execute an already installed operation, specified by the given
%% unique key.
%% @spec spawn_exec([Key], [{atom(), any()}]) -> {reply, {ok, any()}, dictionary()} | {reply, {error}, dictionary()}
spawn_exec([Key], Params) ->
    gen_server:call(?MODULE, {exec, Key, Params});
spawn_exec(Key, Params) ->
    gen_server:call(?MODULE, {exec, Key, Params}).

%% @doc Install and execute a new operation on this peer, and then delete it.
%% @spec remote([{string(), any()}]) -> {reply, {ok, any()}, dictionary()} | {reply, {error}, dictionary()}
remote(Params) ->
    Key = gen_server:call(?MODULE, {spawn, lists:sublist(Params, 4)}),
    Answer = gen_server:call(?MODULE, {exec, Key, lists:sublist(Params, 5, length(Params))}),
    gen_server:cast(?MODULE, {delete, Key}),
    Answer.

%% @doc Add a new process to the internal server list, with the associated key.
%% @spec add_child(string(), pid()) -> {noreply, dictionary()}
add_child(Key, Pid) ->
    gen_server:cast(?MODULE, {add_child, Key, Pid}).

init(_Args) ->
    Spawned = dict:new(),
    {ok, Spawned}.

handle_call({spawn, Params}, _From, Spawned) ->
    F = crest_utils:get_lambda(Params),
    Key = crest_spawn:install(F),
    {reply, Key, Spawned};
handle_call({exec, Key, Params}, _From, Spawned) ->
    case dict:find(Key, Spawned) of
        {ok, ChildPid} ->
            Res = crest_utils:rpc(ChildPid, Params),
            log4erl:info("Executed the existing key ~p~n", [Key]),
            {reply, {ok, Res}, Spawned};
        error ->
            {reply, {error}, Spawned}
    end;
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
