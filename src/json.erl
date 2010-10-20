-module(json).
-export([destructure/2]).

destructure(JS, JSON) ->
    F = destructure_json:parse(JS),
    F(JSON).
