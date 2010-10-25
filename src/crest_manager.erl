%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc Manager module: it invokes all the currently spawned processes,
%% searching for names, and then returns an HTML page with a table of
%% keys and names.
%% @copyright 2010 Alessandro Sivieri

-module(crest_manager).
-compile(export_all).
-export([get_manager/0]).

%% External API

get_manager() ->
    List = crest_peer:get_list({"param", "name"}),
    NewList = lists:map(fun({Key, Name}) -> get_row(Key, Name) end, List),
    Content = get_header(NewList),
    Prolog = ["<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"],
    lists:flatten(xmerl:export_simple([Content], xmerl_xml, [{prolog, Prolog}])).

%% Internal API

get_row(Col1, Col2) ->
    {tr, [], [{td, [], [{a, [{href, Col1}, {title, Col2}], [Col1]}
                        ]},
              {td, [], [Col2]}
              ]}.

get_header(Content) ->
    TableHeader = {tr, [], [{th, [], ["Key"]},
                            {th, [], ["Service name"]}
                           ]},
    Rows = [TableHeader|Content],
    {html, [{xmlns, "http://www.w3.org/1999/xhtml"}],
     [{head, [], [{meta, [{'http-equiv', "Content-Type"}, {content, "text/html; charset=UTF-8"}], []},
                  {title, [], ["CREST - Manager"]}
                  ]},
      {body, [], [{'div', [{id, "main"}], [{h1, [], ["CREST - Manager"]},
                                           {'div', [{id, "proctable"}], [{table, [{id, "processes"}, {border, 1}], Rows}
                                                                         ]}
                                           ]}
                  ]}
      ]
     }.
