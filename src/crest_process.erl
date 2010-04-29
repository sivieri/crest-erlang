%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri 
%% @doc Process operations

-module(crest_process).
-export([install/1, get_lambda_params/2, get_lambda_params/3, get_lambda/1]).

%% External API
install(F) ->
    Key = crest_uuid:uuid(),
    Pid = spawn_link(fun() -> F() end),
    {Key, Pid}.

get_lambda_params(ModuleName, Fun) ->
    get_lambda_params(ModuleName, Fun, []).
get_lambda_params(ModuleName, Fun, OtherList) ->
    {_Name, ModuleBinary, Filename} = code:get_object_code(ModuleName),
    FunBinary = term_to_binary(Fun),
    mochiweb_util:encode(lists:append([{"module", ModuleName}, {"binary", ModuleBinary}, {"filename", Filename}, {"code", FunBinary}], OtherList)).

get_lambda([{"module", ModuleName}, {"binary", ModuleBinary}, {"filename", Filename}, {"code", FunBinary}]) ->
    code:load_binary(ModuleName, Filename, list_to_binary(ModuleBinary)),
    binary_to_term(list_to_binary(FunBinary)).

%% Internal API

