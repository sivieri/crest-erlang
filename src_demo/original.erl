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
-export([get_function/0, get_manager/0, urlsel/0, rss_feed/0]).
-record(widget, {id, url, title = "", x = 0, y = 0, width = 0, height = 0, color = "", host = "", linkto = ""}).

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
            {Pid, {["widget", "manager", "create"], [{"type", "widget"},
                                                     {"id", Id},
                                                     {"title", Title},
                                                     {"x", X},
                                                     {"y", Y},
                                                     {"width", Width},
                                                     {"height", Height},
                                                     {"color", Color},
                                                     {"host", Host}]}} ->
                case Title of
                    "URL Selector" ->
                        Res = crest_operations:install_local("urlsel"),
                        case Res of
                            {ok, Body} ->
                                Widget = #widget{id = Id, url = Body, title = Title, x = X, y = Y, width = Width, height = Height, color = Color, host = Host},
                                NewInstances = dict:store(Id, Widget, Instances),
                                Pid ! {self(), {ok}};
                            {error} ->
                                NewInstances = Instances,
                                Pid ! {self(), {error}}                             
                        end;
                    "RSS Reader" ->
                        Res = crest_operations:install_local("rssfeed"),
                        case Res of
                            {ok, Body} ->
                                Widget = #widget{id = Id, url = Body, title = Title, x = X, y = Y, width = Width, height = Height, color = Color, host = Host},
                                NewInstances = dict:store(Id, Widget, Instances),
                                Pid ! {self(), {ok}};
                            {error} ->
                                NewInstances = Instances,
                                Pid ! {self(), {error}}                                
                        end;
                    _Any ->
                        NewInstances = Instances,
                        Pid ! {self(), {error}}
                end,
                F(F, NewInstances);
            {Pid, {["widget", "manager", "move"], [{"id", Id},
                                                   {"title", _Title},
                                                   {"x", X},
                                                   {"y", Y},
                                                   {"width", Width},
                                                   {"height", Height},
                                                   {"color", Color}]}} ->
                case dict:find(Id, Instances) of
                    {ok, Widget} ->
                        NewWidget = Widget#widget{x = X, y = Y, width = Width, height = Height, color = Color},
                        NewInstances = dict:store(Id, NewWidget, Instances),
                        Pid ! {self(), {ok}};
                    error ->
                        NewInstances = Instances,
                        Pid ! {self(), {error}}
                end,
                F(F, NewInstances);
            {Pid, {["widget", "manager", "link"], [{"type", "link"},
                                                   {"from", IdFrom},
                                                   {"to", IdTo}]}} ->
                case dict:find(IdFrom, Instances) of
                    {ok, Widget} ->
                        NewWidget = Widget#widget{linkto = IdTo},
                        NewInstances = dict:store(IdFrom, NewWidget, Instances),
                        Pid ! {self(), {ok}};
                    error ->
                        NewInstances = Instances,
                        Pid ! {self(), {error}}
                end,
                F(F, NewInstances);
            {Pid, {[{"widget", "manager", "maps"}], _}} ->
                Pid ! {self(), {"application/json", serialize_widgets(Instances)}},
                F(F, Instances);
            Any ->
                io:format("Spawned: ~p~n", [Any]),
                F(F, Instances)
        end
    end,
    fun() ->
        F(F, dict:new())
    end.

urlsel() ->
    F = fun(F, FeedUrl) ->
        receive
            {Pid, [{"id", _Id}, {"url", Url}]} ->
                Pid ! {self(), {ok}},
                F(F, Url);
            {Pid, _} ->
                Answer = {struct, [{erlang:iolist_to_binary("items"), [{struct, [{erlang:iolist_to_binary("url"), erlang:iolist_to_binary(FeedUrl)}]}]}]},
                Pid ! {self(), {"application/json", Answer}},
                F(F, FeedUrl)
        end
    end,
    fun() ->
        F(F, "")
    end.

rss_feed() ->
    F = fun(F) ->
        receive
            {Pid, _} ->
                
                F(F)
        end
    end,
    fun() ->
        F(F)
    end.

%% Internal API

serialize_widgets(Widgets) ->
    SerWidgets = dict:fold(fun(Widget, AccIn) ->
                                   El = {struct, [{erlang:iolist_to_binary("x"), Widget#widget.x},
                                                  {erlang:iolist_to_binary("y"), Widget#widget.y},
                                                  {erlang:iolist_to_binary("url"), erlang:iolist_to_binary("/crest/url/" ++ Widget#widget.url)},
                                                  {erlang:iolist_to_binary("type"), erlang:iolist_to_binary("widget")},
                                                  {erlang:iolist_to_binary("width"), Widget#widget.width},
                                                  {erlang:iolist_to_binary("color"), erlang:iolist_to_binary(Widget#widget.color)},
                                                  {erlang:iolist_to_binary("host"), erlang:iolist_to_binary(Widget#widget.host)},
                                                  {erlang:iolist_to_binary("height"), Widget#widget.height},
                                                  {erlang:iolist_to_binary("title"), erlang:iolist_to_binary(Widget#widget.title)},
                                                  {erlang:iolist_to_binary("id"), erlang:iolist_to_binary(Widget#widget.id)}]},
                                   [El|AccIn]
                                   end, [], Widgets),
    {struct, [{erlang:iolist_to_binary("items"), SerWidgets}]}.

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
