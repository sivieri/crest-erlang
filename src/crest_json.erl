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
%% @doc A module for destructuring JSON Erlang objects (coming from
%% mochijson2:decode()) and selecting inner parts with a Javascript-like
%% syntax.
%% @reference See http://www.progski.net/blog/2010/destructuring_json_in_erlang_made_easy.html
%% @copyright 2010,2011 Alessandro Sivieri

-module(crest_json).
-export([destructure/2]).

%% @doc Call this function with a JSON-decoded structure and
%% a string representing the inner part wanted, and that part
%% is returned.
%% @spec destructure(string(), json_term()) -> any()
destructure(JS, JSON) ->
    F = destructure_json:parse(JS),
    case F(JSON) of
        Value when is_binary(Value) ->
            binary_to_list(Value);
        Value ->
            Value
    end.
