%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri 
%% @doc Process operations module; it is designed as a supervisor_bridge process.
%%      It does not have the capability to restart a child.

-module(crest_process).
-export([install/1, start/1, init/1, terminate/2]).

%% External API
install(F) ->
    Key = crest_uuid:uuid(),
    ChildPid = proc_lib:spawn_link(fun() -> F() end),
    Params = {Key, {?MODULE, start, [ChildPid]}, temporary, infinity, worker, [?MODULE]},
    {ok, _BridgePid} = supervisor:start_child(crest_sup, Params),
    {Key, ChildPid}.

start([ChildPid]) ->
    supervisor_bridge:start_link(?MODULE, [ChildPid]).

init([ChildPid]) ->
    {ok, ChildPid, ChildPid}.

terminate(_Reason, ChildPid) ->
    exit(ChildPid, kill).

%% Internal API

