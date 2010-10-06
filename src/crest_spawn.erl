%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Process operations module; it is designed as a supervisor_bridge process.
%% It does not have the capability to restart a child.
%% @copyright 2010 Alessandro Sivieri 

-module(crest_spawn).
-behaviour(supervisor_bridge).
-export([install/1, start/2, init/1, terminate/2]).

%% External API

%% @doc Install a new operation, specified by the function as parameter,
%% by spawning a new process associated to it (and linked to this bridge);
%% it returns the unique key associated to this function.
%% @spec install(fun()) -> string()
install(F) ->
    Key = uuid:to_string(uuid:srandom()),
    Params = {Key, {?MODULE, start, [Key, F]}, temporary, infinity, supervisor, [?MODULE]},
    supervisor:start_child(crest_sup, Params),
    Key.

%% @doc Start the link of this bridge to the newly spawned function.
%% @spec start(string(), fun()) -> {ok, pid()}
start(Key, F) ->
    {ok, BridgePid} = supervisor_bridge:start_link(?MODULE, {Key, F}),
    {ok, BridgePid}.

%% @doc Add child pid and key to the crest_peer server here, for avoiding
%% problems with shutting down children.
%% @spec init({string(), fun()}) -> {ok, pid(), pid()}
init({Key, F}) ->
    ChildPid = proc_lib:spawn_link(fun() -> F() end),
    crest_peer:add_child(Key, ChildPid),
    {ok, ChildPid, ChildPid}.

%% @doc Terminate a child pid.
%% @spec terminate(any(), pid()) -> atom()
terminate(Reason, ChildPid) ->
    log4erl:info("Supervisor bridge ~p: terminating child ~p (~p)~n", [self(), ChildPid, Reason]),
    exit(ChildPid, Reason).

%% Internal API
