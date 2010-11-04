%% @reference See http://www.progski.net/blog/2010/destructuring_json_in_erlang_made_easy.html

-module(json).
-export([destructure/2]).

destructure(JS, JSON) ->
    F = destructure_json:parse(JS),
    F(JSON).
