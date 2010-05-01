%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc CREST requests router.

-module(crest_router).
-export([route/4]).

%% External API
route(Method, Params, ReqParams, ContentType) ->
    case Method of
        'GET' ->
            case Params of
                ["spawn"] ->
                    {405, [], []};
                ["remote"] ->
                    {405, [], []};
                T ->
                    case crest_peer:spawn_exec(T, ReqParams) of
                        {ok, {CT, Message}} ->
                            {200, [{"Content-Type", CT}], [Message]};
                        {error} ->
                            {404, [], []}
                    end
            end;
        'POST' ->
            case Params of
                ["spawn"] when ContentType =:= "application/x-www-form-urlencoded" ->
                    Key = crest_peer:spawn_install(ReqParams),
                    {200, [{"Content-Type", "text/plain"}], [Key]};
                ["remote"] when ContentType =:= "application/x-www-form-urlencoded" ->
                    case crest_peer:remote(ReqParams) of
                        {ok, {CT, Message}} ->
                            {200, [{"Content-Type", CT}], [Message]};
                        {error} ->
                            {404, [], []}
                    end;
                T ->
                    % POST request for a spawned app
                    case crest_peer:spawn_exec(T, ReqParams) of
                        {ok, {CT, Message}} ->
                            {200, [{"Content-Type", CT}], [Message]};
                        {error} ->
                            {404, [], []}
                    end
            end;
        _ ->
            {405, [], []}
    end.

%% Internal API

