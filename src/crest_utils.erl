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
-export([ssl_options/0, format/2, rpc/2, first/1, pmap/2, get_lambda_params/3, get_lambda/1, invoke_spawn/3, invoke_remote/4, invoke_lambda/3, code_hash/1]).

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

%% @doc Take a list of parameters from an HTTP request and recreate the binary fun that
%% is encoded there.
%% Attention: because of code load, if the same module is sent to the server
%% more than two times, then the older processes are killed (cfr. the standard
%% Erlang policy); thus, maybe some version control has to be inserted (if possible),
%% or remote closures have to be spawned inside some update-savvy module (well, I
%% don't think this solves the problem, but anyway...).
%% Remember: this is not a bug, it's a feature!
%% @spec get_lambda([{string(), any()}]) -> term()
get_lambda([{"module", ModuleName}, {"binary", ModuleBinary}, {"hash", ModuleHash}, {"filename", Filename}, {"code", FunBinary}]) ->
    code:load_binary(list_to_atom(ModuleName), Filename, list_to_binary(ModuleBinary)),
    binary_to_term(list_to_binary(FunBinary)).

%% @doc Spawn a function on a certain host; the function module needs to be
%% in the Erlang path.
%% @spec invoke_spawn(string(), atom(), atom()) -> {ok, Key} | {error}
invoke_spawn(Host, Module, Function) ->
	inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://" ++ Host ++ ":8443/crest/spawn", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(Module, Module:Function(), [])}, [crest_utils:ssl_options()], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

%% @doc Spawn a function on a certain host and invoke it with parameters;
%% the function module needs to be in the Erlang path.
%% @spec invoke_remote(string(), atom(), atom(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_remote(Host, Module, Function, Params) ->
	inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"https://" ++ Host ++ ":8443/crest/remote", [], "application/x-www-form-urlencoded", crest_utils:get_lambda_params(Module, Module:Function(), Params)}, [crest_utils:ssl_options()], []),
	io:format("~p~n", [Res]),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

%% @doc Invoke an already installed computation with parameters.
%% @spec invoke_lambda(string(), string(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_lambda(Host, Key, Params) ->
	inets:start(),
	ssl:start(),
    Res = httpc:request(post, {"http://"++ Host ++ ":8080/crest/url/" ++ Key, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode(Params)}, [], []),
    case Res of
        {ok, {{_,200,_}, _, Body}} ->
            {ok, Body};
		{ok, {{_,_,_}, _, _}} ->
			{error};
        {error, _Reason} ->
            {error}
    end.

%% @doc Calculate the hash of a binary blob; it hides the hash
%% function used, which may change over time, while at the same
%% time it returns the string representation instead of a binary
%% list (as the crypto module does).
%% @spec code_hash(binary()) -> string()
code_hash(Binary) ->
	lists:flatten([io_lib:format("~2.16.0b",[N])||N<-binary_to_list(erlang:md5(Binary))]).

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

get_lambda_params(ModuleName, Fun, OtherList) ->
    {_Name, ModuleBinary, Filename} = code:get_object_code(ModuleName),
    FunBinary = term_to_binary(Fun),
    mochiweb_util:urlencode(lists:append([{"module", ModuleName}, {"binary", ModuleBinary}, {"hash", code_hash(ModuleBinary)}, {"filename", Filename}, {"code", FunBinary}], OtherList)).
