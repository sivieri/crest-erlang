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
-export([pack_key/1, ssl_options/0, format/2, rpc/2, first/1, pmap/2, get_lambda_params/3, get_lambda/1, code_hash/1, http_get/1]).

%% External API

%% @doc Put a key in its correct JSON format, for sending it away.
%% @spec pack_key(string()) -> {struct, [{binary(), binary()}]}
pack_key(Key) ->
    {struct, [{erlang:iolist_to_binary("Key"), erlang:iolist_to_binary(Key)}]}.

%% @doc Get the content of the given URL.
%% @spec http_get(string()) -> {ok, string()} | error
http_get(Url) ->
    ibrowse:start(),
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _, Body} ->
            {ok, Body};
        {ok, _, _, _} ->
            error;
        {error, _Reason} ->
            error
    end.

%% @doc Returns the list of SSL options for making HTTPS requests with
%% mutual authentication.
%% @spec ssl_options() -> [any()]
ssl_options() ->
	[{is_ssl, true},
	{ssl_options,
	 [{verify, 0},
	  {certfile, crest_deps:local_path(["ca", "users", "peer", "peer.crt"])},
	  {keyfile, crest_deps:local_path(["ca", "users", "peer", "peer.key"])},
	  {password, "peer"}]
	}].

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
%% is encoded there. This function does not load the same module twice, unless the hash
%% value of the binary code is changed.
%% @spec get_lambda([{string(), any()}]) -> term()
get_lambda([{"module", ModuleName}, {"binary", ModuleBinary}, {"hash", ModuleHash}, {"filename", Filename}, {"code", FunBinary}]) ->
	ModuleAtom = list_to_atom(ModuleName),
	ModuleRealBinary = list_to_binary(ModuleBinary),
	case code:is_loaded(ModuleAtom) of
		{file, _} ->
			{_, OldModuleBinary, _} = code:get_object_code(ModuleAtom),
			OldHash = code_hash(OldModuleBinary),
			case string:equal(OldHash, ModuleHash) of
				true ->
					binary_to_term(list_to_binary(FunBinary));
				false ->
					log4erl:info("Updating module bytecode: ~p~n", [ModuleName]),
					code:load_binary(ModuleAtom, Filename, ModuleRealBinary),
    				binary_to_term(list_to_binary(FunBinary))
			end;
		false ->
    		code:load_binary(ModuleAtom, Filename, ModuleRealBinary),
    		binary_to_term(list_to_binary(FunBinary))
	end.

%% @doc Calculate the hash of a binary blob; it hides the hash
%% function used, which may change over time, while at the same
%% time it returns the string representation instead of a binary
%% list (as the crypto module does).
%% @spec code_hash(binary()) -> string()
code_hash(Binary) ->
	lists:flatten([io_lib:format("~2.16.0b",[N])||N<-binary_to_list(erlang:md5(Binary))]).

get_lambda_params(ModuleName, Fun, OtherList) ->
    {_Name, ModuleBinary, Filename} = code:get_object_code(ModuleName),
    FunBinary = term_to_binary(Fun),
    mochiweb_util:urlencode(lists:append([{"module", ModuleName}, {"binary", ModuleBinary}, {"hash", code_hash(ModuleBinary)}, {"filename", Filename}, {"code", FunBinary}], OtherList)).

%% Internal API

do_f(Parent, Ref, F, I) -> 
    Parent ! {Ref, F(I)}.

gather(0, _, L) ->
	L;
gather(N, Ref, L) ->
    receive
    	{Ref, Ret} ->
			gather(N-1, Ref, [Ret|L])
    end.
