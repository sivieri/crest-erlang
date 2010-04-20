%% @author Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
%% @copyright 2010 Alessandro Sivieri
%% @doc Main launcher.

-module(crest_server).
-behaviour(gen_server).
-export([start/0, stop/0, spawn_install/1, remote/1, spawn_exec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% External API
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

spawn_install(Params) ->
    gen_server:call(?MODULE, {spawn, Params}).

spawn_exec([Key], Params) ->
    gen_server:call(?MODULE, {exec, Key, Params});
spawn_exec(Key, Params) ->
    gen_server:call(?MODULE, {exec, Key, Params}).

remote([Param|T]) ->
    Key = gen_server:call(?MODULE, {spawn, Param}),
    Answer = gen_server:call(?MODULE, {exec, Key, T}),
    gen_server:cast(?MODULE, {delete, Key}),
    Answer.

init(_Args) ->
    {ok, dict:new()}.

handle_call({spawn, Params}, _From, State) ->
    {"code", Code} = hd(Params),
    F = binary_to_term(list_to_binary(Code)),
    {Key, Pid2} = crest_process:install(F),
    error_logger:info_msg("Registered a new key ~p~n", [Key]),
    {reply, Key, [dict:append(Key, Pid2, State)]};
handle_call({exec, Key, Params}, _From, State) ->
    case dict:find(Key, State) of
        {ok, Pid2} ->
            Res = crest_utils:rpc(Pid2, Params),
            error_logger:info_msg("Executed the existing key ~p~n", [Key]),
            {reply, {ok, Res}, State};
        error ->
            {reply, {error}, State}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({delete, Key}, State) ->
    error_logger:info_msg("Deleted the key ~p~n", [Key]),
    {noreply, dict:erase(Key, State)};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:warning_msg("The spawned process ~p exited: ~p~n", [Pid, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal API
