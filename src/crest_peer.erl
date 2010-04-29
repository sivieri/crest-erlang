%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Main launcher.

-module(crest_peer).
-behaviour(gen_server).
-export([start/0, stop/0, spawn_install/1, remote/1, spawn_exec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% External API
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

spawn_install(Params) ->
    gen_server:call(?MODULE, {spawn, Params}).

spawn_exec([Key], Params) ->
    gen_server:call(?MODULE, {exec, Key, Params});
spawn_exec(Key, Params) ->
    gen_server:call(?MODULE, {exec, Key, Params}).

remote(Params) ->
    Key = gen_server:call(?MODULE, {spawn, lists:sublist(Params, 4)}),
    Answer = gen_server:call(?MODULE, {exec, Key, lists:sublist(Params, 5, length(Params))}),
    gen_server:cast(?MODULE, {delete, Key}),
    Answer.

init(_Args) ->
    Spawned = dict:new(),
    {ok, Spawned}.

handle_call({spawn, Params}, _From, Spawned) ->
    F = crest_process:get_lambda(Params),
    {Key, Pid2} = crest_process:install(F),
    NewSpawned = dict:store(Key, Pid2, Spawned),
    log4erl:info("Registered a new key ~p~n", [Key]),
    {reply, Key, NewSpawned};
handle_call({exec, Key, Params}, _From, Spawned) ->
    case dict:find(Key, Spawned) of
        {ok, Pid2} ->
            Res = crest_utils:rpc(Pid2, Params),
            log4erl:info("Executed the existing key ~p~n", [Key]),
            {reply, {ok, Res}, Spawned};
        error ->
            {reply, {error}, Spawned}
    end;
handle_call(_Request, _From, Spawned) ->
    {noreply, Spawned}.

handle_cast({delete, Key}, Spawned) ->
    NewSpawned = dict:erase(Key, Spawned),
    log4erl:info("Deleted the key ~p~n", [Key]),
    {noreply, NewSpawned};
handle_cast(_Request, Spawned) ->
    {noreply, Spawned}.

handle_info({'EXIT', Pid, Reason}, Spawned) ->
    log4erl:warn("The spawned process ~p exited: ~p~n", [Pid, Reason]),
    {noreply, Spawned};
handle_info(_Info, Spawned) ->
    {noreply, Spawned}.

code_change(_OldVsn, Spawned, _Extra) ->
    {ok, Spawned}.

terminate(_Reason, _Spawned) ->
    ok.

%% Internal API
