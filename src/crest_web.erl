%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Web server for crest.

-module(crest_web).
-export([start/1, stop/0, loop/2]).

%% External API
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    ContentType = Req:get_header_value("content-type"),
    error_logger:info_msg("Request: ~p~n", [Path]),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case string:tokens(Path, "/") of
                ["crest"|"spawn"] ->
                    Req:respond({405, [], []});
                ["crest"|"remote"] ->
                    Req:respond({405, [], []});
                ["crest"|["demo"]] ->
                    Req:respond({200, [{"Content-Type", "text/html"}], [crest_demo:start_demo()]});
                ["crest"|["demo2"]] ->
                    case crest_demo:spawn_demo() of
                        {ok, Message} ->
                            Req:respond({200, [{"Content-Type", "text/html"}], [Message]});
                        {error} ->
                            Req:respond({500, [], []})
                    end;
                ["crest"|T] ->
                    Params = Req:parse_qs(),
                    case crest_server:spawn_exec(T, Params) of
                        {ok, {CT, Message}} ->
                            Req:respond({200, [{"Content-Type", CT}], [Message]});
                        {error} ->
                            Req:not_found()
                    end;
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case string:tokens(Path, "/") of
                ["crest"|["spawn"]] when ContentType =:= "application/x-www-form-urlencoded" ->
                    Params = Req:parse_post(),
                    Key = crest_server:spawn_install(Params),
                    Req:respond({200, [{"Content-Type", "text/plain"}], [Key]});
                ["crest"|["remote"]] when ContentType =:= "application/x-www-form-urlencoded" ->
                    Params = Req:parse_post(),
                    case crest_server:remote(Params) of
                        {ok, {CT, Message}} ->
                            Req:respond({200, [{"Content-Type", CT}], [Message]});
                        {error} ->
                            Req:not_found()
                    end;
                ["crest"|T] ->
                    % POST request for a spawned app
                    Params = Req:parse_post(),
                    case crest_server:spawn_exec(T, Params) of
                        {ok, {CT, Message}} ->
                            Req:respond({200, [{"Content-Type", CT}], [Message]});
                        {error} ->
                            Req:not_found()
                    end;
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
