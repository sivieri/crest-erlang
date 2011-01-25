%% @author Someauthor <somemail@somedomain.xx>
%% @doc Sample module
%% @copyright yyyy Someauthor

-module(module_sample).
-export([my_service/0]).

%% External API

my_service(State) ->
    receive
        {Pid, [{"par1", P1}, {"par2", P2}, ...]} ->
            %% Do your job accessing par1, ... parN
            %% eventually create a new state NewState
            
            %% Finish with a tail recursion
            my_service(NewState)  
            %% Or spawn myself on peer Hostname
            invoke_spawn(Hostname, ?MODULE, fun() -> my_service(NewState) end) 
    end.

%% Internal API
