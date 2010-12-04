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
%% @doc Miscellaneous utilities.
%% @copyright 2010 Alessandro Sivieri

-module(crest_utils).
-export([ssl_options/0, format/2, rpc/2, first/1, pmap/2, get_lambda_params/2, get_lambda_params/3, get_lambda/1]).

%% External API

ssl_options() ->
	{ssl,
	 [{verify, 0},
	  {certfile, crest_deps:local_path(["ca", "users", "peer", "peer.crt"])},
	  {keyfile, crest_deps:local_path(["ca", "users", "peer", "peer.key"])},
	  {password, "peer"}]
	}.

%% @doc Returns the head of a list if the parameter is a list, or
%% the parameter itself if it is not a list.
%% @spec first([any()]) -> any()
first(Params) ->
    case is_list(Params) of
        true ->
            hd(Params);
        false ->
            Params
    end.

%% @doc Format the parameters as a string.
%% @spec format(string(), [term()]) -> string()
format(String, Elements) ->
    Pass = io_lib:format(String, Elements),
    lists:flatten(Pass).

%% @doc Remote Procedure Call: send a message to a certain pid and wait for a response.
%% @spec rpc(pid(), any()) -> any()
rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {_, Response} ->
            Response
    end.

%% @doc Parallel version of the map/2 function.
%% @spec pmap(fun(), [any()]) -> [any()]
pmap(F, L) -> 
    S = self(),
    Ref = erlang:make_ref(), 
    lists:foreach(fun(I) -> 
               spawn(fun() -> do_f(S, Ref, F, I) end)
           end, L),
    gather(length(L), Ref,  []).

%% @doc Combine a set of parameters to an urlencoded list, to be transmitted over
%% HTTP.
%% @spec get_lambda_params(atom(), fun()) -> string()
get_lambda_params(ModuleName, Fun) ->
    get_lambda_params(ModuleName, Fun, []).
%% @spec get_lambda_params(atom(), fun(), [any()]) -> string()
get_lambda_params(ModuleName, Fun, OtherList) ->
    {_Name, ModuleBinary, Filename} = code:get_object_code(ModuleName),
    FunBinary = term_to_binary(Fun),
    mochiweb_util:urlencode(lists:append([{"module", ModuleName}, {"binary", ModuleBinary}, {"filename", Filename}, {"code", FunBinary}], OtherList)).

%% @doc Take a list of parameters from an HTTP request and recreate the binary fun that
%% is encoded there.
%% Attention: because of code load, if the same module is sent to the server
%% more than two times, then the older processes are killed (cfr. the standard
%% Erlang policy); thus, maybe some version control has to be inserted (if possible),
%% or remote closures have to be spawned inside some update-savvy module (well, I
%% don't think this solves the problem, but anyway...).
%% Remember: this is not a bug, it's a feature!
%% @spec get_lambda([{string(), any()}]) -> term()
get_lambda([{"module", ModuleName}, {"binary", ModuleBinary}, {"filename", Filename}, {"code", FunBinary}]) ->
    code:load_binary(list_to_atom(ModuleName), Filename, list_to_binary(ModuleBinary)),
    binary_to_term(list_to_binary(FunBinary)).

%% Internal API

do_f(Parent, Ref, F, I) ->                      
    Parent ! {Ref, (catch F(I))}.

gather(0, _, L) ->
	L;
gather(N, Ref, L) ->
    receive
    	{Ref, Ret} ->
			gather(N-1, Ref, [Ret|L])
    end.
