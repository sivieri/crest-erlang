%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri 
%% @doc Process operations module; it is designed as a supervisor_bridge process.
%%      It does not have the capability to restart a child.

-module(crest_process).
-export([install/1, start/2, init/1, terminate/2]).

%% External API
install(F) ->
    Key = uuid:to_string(uuid:srandom()),
    Params = {Key, {?MODULE, start, [Key, F]}, temporary, infinity, supervisor, [?MODULE]},
    supervisor:start_child(crest_sup, Params),
    Key.

start(Key, F) ->
    {ok, BridgePid} = supervisor_bridge:start_link(?MODULE, {Key, F}),
    {ok, BridgePid}.

% Add child pid and key to the crest_peer server here, for avoiding
% problems with shutting down children.
init({Key, F}) ->
    ChildPid = proc_lib:spawn_link(fun() -> F() end),
    crest_peer:add_child(Key, ChildPid),
    {ok, ChildPid, ChildPid}.

terminate(Reason, ChildPid) ->
    log4erl:info("Terminating ~p: ~p~n", [ChildPid, Reason]),
    exit(ChildPid, Reason).

%% Internal API

