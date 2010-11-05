%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @doc A module for destructuring JSON Erlang objects (coming from
%% mochijson2:decode()) and selecting inner parts with a Javascript-like
%% syntax.
%% @reference See http://www.progski.net/blog/2010/destructuring_json_in_erlang_made_easy.html
%% @copyright 2010 Alessandro Sivieri

-module(crest_json).
-export([destructure/2]).

%% @doc Call this function with a JSON-decoded structure and
%% a string representing the inner part wanted, and that part
%% is returned.
%% @spec destructure(string(), json_term()) -> iolist()
destructure(JS, JSON) ->
    F = destructure_json:parse(JS),
    F(JSON).
