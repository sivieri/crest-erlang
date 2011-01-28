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
                Pid ! {self(), "GET"},
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
            {Pid, {["widget", "manager", "create"], [], Body}} ->
                NewInstances = do_create(Pid, Instances, Body),
                F(F, NewInstances);
            {Pid, {["widget", "manager", "move"], [], Body}} ->
                NewInstances = do_move(Pid, Instances, Body),
                F(F, NewInstances);
            {Pid, {["widget", "manager", "link"], [], Body}} ->
                NewInstances = do_link(Pid, Instances, Body),
                F(F, NewInstances);
            {Pid, {["widget", "manager", "maps"], _}} ->
                spawn(fun() -> do_serialize(Pid, Instances) end),
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
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "URL selector"},
                F(F, FeedUrl);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET/POST"},
                F(F, FeedUrl);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), []},
                F(F, FeedUrl);
            {Pid, {[], Body}} ->
                Obj = mochijson2:decode(Body),
                Url = crest_json:destructure("Obj.url", Obj),
                Pid ! {self(), {ok}},
                F(F, Url);
            {Pid, _} ->
                spawn(fun() ->
                    Answer = {struct, [{erlang:iolist_to_binary("items"), [{struct, [{erlang:iolist_to_binary("url"), erlang:iolist_to_binary(FeedUrl)}]}]}]},
                    Pid ! {self(), {"application/json", mochijson2:encode(Answer)}} end),
                F(F, FeedUrl)
        end
    end,
    fun() ->
        F(F, "http://localhost:8080/original/feed.xml")
    end.

rss_feed() ->
    F = fun(F, UrlSel) ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "RSS feed loader"},
                F(F, UrlSel);
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "GET/POST"},
                F(F, UrlSel);
            {Pid, {"param", "parameters"}} ->
                Pid ! {self(), []},
                F(F, UrlSel);
            {Pid, [{"link", Key}]} ->
                Pid ! {self(), ok},
                F(F, Key);
            {Pid, _} when length(UrlSel) > 0 ->
                spawn(fun() ->
                    case crest_operations:invoke_local_lambda(UrlSel, []) of
                        {ok, {_CT, Bin}} ->
                            Obj = mochijson2:decode(Bin),
                            Url = crest_json:destructure("Obj.items[0].url", Obj),
                            case crest_utils:http_get(Url) of
                                {ok, Feed} ->
                                    Res = feed_to_json(parse_feed(Feed)),
                                    Pid ! {self(), {"application/json", mochijson2:encode(Res)}};
                                error ->
                                    Pid ! {self(), {error}}
                            end;
                        {error} ->
                            Pid ! {self(), {error}}
                    end end),
                F(F, UrlSel);
            {Pid, _} ->
                Pid ! {self(), {"application/json", mochijson2:encode(feed_to_json([]))}},
                F(F, UrlSel)
        end
    end,
    fun() ->
        F(F, "")
    end.

%% Internal API

serialize_widgets(Widgets) ->
    SerWidgets = dict:fold(fun(_Id, Widget, AccIn) ->
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
    Feeds = lists:map(fun({Title, Date, Desc}) ->
                               {struct, [{erlang:iolist_to_binary("type"), erlang:iolist_to_binary("message")},
                                         {erlang:iolist_to_binary("label"), erlang:iolist_to_binary(Title)},
                                         {erlang:iolist_to_binary("sent"), erlang:iolist_to_binary(Date)},
                                         {erlang:iolist_to_binary("text"), erlang:iolist_to_binary(Desc)}
                                         ]}
                               end, List),
    {struct, [{erlang:iolist_to_binary("items"), Feeds}]}.

%% @reference http://www.trapexit.org/How_to_write_an_RSS_aggregator
parse_feed(Content) ->
    {Doc, _Misc} = xmerl_scan:string(Content),
    Items = getElementsByTagName(Doc, item),
    lists:map(fun(Item) ->
                      {textOf(first(Item, title)),
                       textOf(first(Item, pubDate)),
                       textOf(first(Item, description))}
                      end, Items).

getElementsByTagName([H|T], Item) when H#xmlElement.name == Item ->
    [H | getElementsByTagName(T, Item)];
getElementsByTagName([H|T], Item) when is_record(H, xmlElement) ->
    getElementsByTagName(H#xmlElement.content, Item) ++
      getElementsByTagName(T, Item);                                                                  
getElementsByTagName(X, Item) when is_record(X, xmlElement) ->
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

do_create(Pid, Instances, Body) ->
    Obj = mochijson2:decode(Body),
    Id = crest_json:destructure("Obj.id", Obj),
    Title = crest_json:destructure("Obj.title", Obj),
    X = crest_json:destructure("Obj.x", Obj),
    Y = crest_json:destructure("Obj.y", Obj),
    Width = crest_json:destructure("Obj.width", Obj),
    Height = crest_json:destructure("Obj.height", Obj),
    Color = crest_json:destructure("Obj.color", Obj),
    Host = crest_json:destructure("Obj.host", Obj),
    case Title of
        "URL Selector" ->
            Res = crest_operations:install_local("urlsel"),
            case Res of
                {ok, UUID} ->
                    Widget = #widget{id = Id, url = UUID, title = Title, x = X, y = Y, width = Width, height = Height, color = Color, host = Host},
                    Pid ! {self(), {ok}},
                    dict:store(Id, Widget, Instances);
                {error} ->
                    Pid ! {self(), {error}},
                    Instances
            end;
        "RSS Reader" ->
            Res = crest_operations:install_local("rssfeed"),
            case Res of
                {ok, UUID} ->
                    Widget = #widget{id = Id, url = UUID, title = Title, x = X, y = Y, width = Width, height = Height, color = Color, host = Host},
                    Pid ! {self(), {ok}},
                    dict:store(Id, Widget, Instances);
                {error} ->
                    Pid ! {self(), {error}},
                    Instances
            end;
        _Any ->
               Pid ! {self(), {error}},
               Instances
    end.

do_move(Pid, Instances, Body) ->
    Obj = mochijson2:decode(Body),
    Id = crest_json:destructure("Obj.id", Obj),
    X = crest_json:destructure("Obj.x", Obj),
    Y = crest_json:destructure("Obj.y", Obj),
    Width = crest_json:destructure("Obj.width", Obj),
    Height = crest_json:destructure("Obj.height", Obj),
    Color = crest_json:destructure("Obj.color", Obj),
    case dict:find(Id, Instances) of
        {ok, Widget} ->
            NewWidget = Widget#widget{x = X, y = Y, width = Width, height = Height, color = Color},
            Pid ! {self(), {ok}},
            dict:store(Id, NewWidget, Instances);
        error ->
            Pid ! {self(), {error}},
            Instances
    end.

do_link(Pid, Instances, Body) ->
    Obj = mochijson2:decode(Body),
    IdFrom = crest_json:destructure("Obj.from", Obj),
    IdTo = crest_json:destructure("Obj.to", Obj),
    case dict:find(IdFrom, Instances) of
        {ok, WidgetFrom} ->
            case dict:find(IdTo, Instances) of
                {ok, WidgetTo} ->
                    NewWidget = WidgetFrom#widget{linkto = IdTo},
                    case crest_operations:invoke_local_lambda(WidgetTo#widget.url, [{"link", WidgetFrom#widget.url}]) of
                        {ok, _Res} ->
                            Pid ! {self(), {ok}},
                            dict:store(IdFrom, NewWidget, Instances);
                        {error} ->
                            Pid ! {self(), {error}},
                            Instances
                    end;
                {error} ->
                    Pid ! {self(), {error}},
                    Instances
            end;
        error ->
            Pid ! {self(), {error}},
            Instances
     end.

do_serialize(Pid, Instances) ->
    Pid ! {self(), {"application/json", mochijson2:encode(serialize_widgets(Instances))}}.
