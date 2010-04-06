%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri 
%% @doc Process operations

-module(crest_process).
-export([install/1, parse_closure/1]).

%% External API
install(F) ->
    Key = crest_uuid:uuid(),
    Pid = spawn_link(fun() -> F() end),
    {Key, Pid}.

parse_closure(F) ->
    true.
