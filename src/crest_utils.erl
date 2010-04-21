%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Miscellaneous utilities.

-module(crest_utils).
-export([format/2, rpc/2, first/1]).

%% External API
first(Params) ->
    case is_list(Params) of
        true ->
            hd(Params);
        false ->
            Params
    end.

format(String, Elements) ->
    Pass = io_lib:format(String, Elements),
    lists:flatten(Pass).

rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {_, Response} ->
            Response
    end.

%% Internal API
