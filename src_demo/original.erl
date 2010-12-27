%% Copyright (c) 2010 Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% 
%% This file is part of CREST-Erlang.
%% 
%% CREST-Erlang is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% CREST-Erlang is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with CREST-Erlang. If not, see <http://www.gnu.org/licenses/>.
%% 
%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc The original demo module.
%% @copyright 2010 Alessandro Sivieri

-module(original).
-include_lib("xmerl/include/xmerl.hrl").
-export([get_function/0, get_manager/0]).
-record(widget, {wid, pid, x = 0, y = 0}).
-compile(export_all).

%% External API

get_function() ->
    F = fun(F, Instances) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Original demo instances list"},
                F(F, Instances);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET/POST"},
                F(F, Instances);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"instance", "string()"}]},
                F(F, Instances);
            {Pid, [{"instance", "new"}]} ->
                Res = crest_operations:install_local("manager"),
                case Res of
                    {ok, Body} ->
                        NewInstances = [Body|Instances],
                        Pid ! {self(), {"text/plain", Body}},
                        F(F, NewInstances);
                    {error} ->
                        Pid ! {self(), {"text/plain", crest_utils:format("Error")}},
                        F(F, Instances)
                end;
            {Pid, []} ->
                case Instances of
                    [H|_] ->
                        Pid ! {self(), {"text/plain", H}};
                    [] ->
                        Res = crest_operations:install_local("manager"),
                        case Res of
                            {ok, Body} ->
                                NewInstances = [Body|Instances],
                                Pid ! {self(), {"text/plain", Body}},
                                F(F, NewInstances);
                            {error} ->
                                Pid ! {self(), {"text/plain", crest_utils:format("Error")}}
                        end
                end,
                F(F, Instances);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F, Instances)
        end
    end,
    fun() ->
        F(F, [])
    end.

get_manager() ->
    F = fun(F, Instances) ->
        process_flag(trap_exit, true),
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "Original demo main manager"},
                F(F, Instances);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET/POST"},
                F(F, Instances);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), []},
                F(F, Instances);
            {Pid, []} ->
                F(F, Instances);
            {'EXIT', Pid, Reason} ->
                F(F, Instances);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F, Instances)
        end
    end,
    fun() ->
        F(F, [])
    end.

%% Internal API

urlsel() ->
    F = fun(F, FeedUrl) ->
        receive
            {_Pid, [{"url", NewUrl}]} ->
                F(F, NewUrl);
            {Pid, geturl} ->
                Pid ! {self(), FeedUrl},
                F(F, FeedUrl);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                F(F, FeedUrl)
        end
    end,
    fun() ->
        F(F, "")
    end.

rss_feed() ->
    F = fun(F) ->
        receive
            {Pid, [{"getrss", Url}]} ->
                case crest_utils:http_get(Url) of
                    {ok, Content} ->
                        Elements = feed_to_json(parse_feed(Content)),
                        Pid ! {self(), {"application/json", mochijson2:encode(Elements)}};
                    error ->
                        Pid ! {self(), {"text/plain", "Return some relevant result here."}}
                end,
                F(F);
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

feed_to_json(List) ->
    lists:map(fun({Title, Date, Desc}) ->
                               {struct, [{erlang:iolist_to_binary("label"), erlang:iolist_to_binary(Title)},
                                         {erlang:iolist_to_binary("sent"), erlang:iolist_to_binary(Date)},
                                         {erlang:iolist_to_binary("text"), erlang:iolist_to_binary(Desc)}
                                         ]}
                               end, List).

parse_feed(Content) ->
    {Doc, _Misc} = xmerl_scan:string(Content),
    Items = getElementsByTagName(Doc, item),
    lists:map(fun(Item) ->
                      {textOf(first(Item, title)),
                       textOf(first(Item, 'dc:date')),
                       textOf(first(Item, description))}
                      end, Items).

getElementsByTagName([H|T], Item) when H#xmlElement.name == Item ->
    [H | getElementsByTagName(T, Item)];
getElementsByTagName([H|T], Item) when record(H, xmlElement) ->
    getElementsByTagName(H#xmlElement.content, Item) ++
      getElementsByTagName(T, Item);                                                                  
getElementsByTagName(X, Item) when record(X, xmlElement) ->
    getElementsByTagName(X#xmlElement.content, Item);
getElementsByTagName([_|T], Item) ->
    getElementsByTagName(T, Item);
getElementsByTagName([], _) ->
    [].

first(Item, Tag) ->
    hd([X || X <- Item#xmlElement.content,
         X#xmlElement.name == Tag]).

textOf(Item) ->
    lists:flatten([X#xmlText.value || X <- Item#xmlElement.content,
                      element(1,X) == xmlText]).
