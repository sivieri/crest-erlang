%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Supervisor for the crest application.

-module(crest_sup).
-behaviour(supervisor).
-export([start_link/0, upgrade/0]).
-export([init/1]).

%% External API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

init([]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,   
    WebConfig = [
         {ip, Ip},
                 {port, 8001},
                 {docroot, crest_deps:local_path(["priv", "www"])}],
    Web = {crest_web,
           {crest_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    Peer = {crest_peer,
            {crest_peer, start, []},
            permanent, 5000, worker, [crest_peer]},

    Processes = [Web, Peer],
    {ok, {{one_for_one, 10, 10}, Processes}}.
