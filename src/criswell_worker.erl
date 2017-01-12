%% @doc This is the worker process - responsible for executing the computation
%% and sending the result back to a manager.
-module(criswell_worker).

-export([
    run/2
]).

run(F, MgrPid) when is_function(F) ->
    Result = F(),
    MgrPid ! {value, Result};

run({M,F,A}, MgrPid) ->
    Result = M:F(A),
    MgrPid ! {value, Result};

run(_Other, MgrPid) ->
    MgrPid ! {error, bad_func}.
