%% Copyright (c) 2010,2011 Alessandro Sivieri <sivieri@elet.polimi.it>
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
%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Load tests.
%% @copyright 2010,2011 Alessandro Sivieri

-module(test_load).
-export([factory/0, short/0, long/0]).

factory() ->
	receive
        {Pid, {"param", "name"}} ->
            Pid ! {self(), "Load test manager"},
            factory();
        {Pid, {"param", "operation"}} ->
            Pid ! {self(), "GET"},
            factory();
        {Pid, {"param", "parameters"}} ->
            Pid ! {self(), [{"service", "string()"}, {"port", "integer()"}]},
            factory();
        {Pid, [{"service", Input}, {"port", Port}]} ->
            case Input of
				"short" ->
					case crest_operations:invoke_spawn("localhost", list_to_integer(Port), ?MODULE, short) of
						{ok, Key} ->
							Pid ! {self(), {"text/plain", Key}};
						{error} ->
							Pid ! {self(), {"text/plain", "Unable to spawn to localhost."}}
					end;
				"long" ->
					case crest_operations:invoke_spawn("localhost", list_to_integer(Port), ?MODULE, long) of
						{ok, Key} ->
							Pid ! {self(), {"text/plain", Key}};
						{error} ->
							Pid ! {self(), {"text/plain", "Unable to spawn to localhost."}}
					end;
				_ ->
					Pid ! {self(), {"text/plain", "Input must be long/short."}}
			end,
            factory();
        Any ->
            io:format("Spawned: ~p~n", [Any]),
            factory()
    end.

short() ->
    receive
        {Pid, {"param", "name"}} ->
            Pid ! {self(), "Short computation"},
            short();
        {Pid, {"param", "operation"}} ->
            Pid ! {self(), "GET"},
            short();
        {Pid, {"param", "parameters"}} ->
            Pid ! {self(), [{"input", "string()"}]},
            short();
        {Pid, [{"input", Input}]} ->
            spawn(fun() -> handle_short(Pid, Input) end),
            short();
        Any ->
            io:format("Spawned: ~p~n", [Any]),
            short()
    end.

long() ->
    receive
        {Pid, {"param", "name"}} ->
            Pid ! {self(), "Long computation"},
            long();
        {Pid, {"param", "operation"}} ->
            Pid ! {self(), "GET"},
            long();
        {Pid, {"param", "parameters"}} ->
            Pid ! {self(), [{"input", "string()"}]},
            long();
        {Pid, [{"input", InputString}]} ->
            spawn(fun() -> handle_long(Pid, InputString) end),
            long();
        Any ->
            io:format("Spawned: ~p~n", [Any]),
            long()
    end.

% Internal API

handle_long(Pid, Param) ->
    OutBin = lists:foldl(fun(_Elem, AccIn) ->
                             crypto:md5(AccIn)
                             end, Param, lists:seq(1, 10)),
    OutString = lists:flatten([io_lib:format("~2.16.0b",[N])||N<-binary_to_list(OutBin)]),
    Pid ! {self(), {"text/plain", OutString}}.

handle_short(Pid, Param) ->
    OutString = lists:reverse(Param),
    Pid ! {self(), {"text/plain", OutString}}.
