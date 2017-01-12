%% @doc This module implements the listener/receiver for a worker.
-module(criswell_mgr).

-include("criswell.hrl").

-export([
    start_link/3
]).

start_link(Timeout, Func, Ref) ->
    Pid = spawn_link(fun() -> manage(Timeout, Func, Ref) end),
    {ok, Pid}.

manage(Timeout, F, Ref) ->
    MgrPid = self(),
    {Pid, Mref} = spawn_monitor(fun() -> criswell_worker:run(F, MgrPid) end),

    receive
        {value, V} -> update(Ref, V);
        {'DOWN', Mref, process, Pid, normal} -> ok;
        {'DOWN', Mref, process, Pid, Info} -> update(Ref, Info);
        Other -> error_logger:warning_msg("Unexpected message ~p received by ~p", [Other, self()])
    after Timeout ->
        demonitor(Mref),
        exit(Pid, kill), %% clean up after ourselves
        update(Ref, timeout)
    end.

%% ETS guarantees that update_element is atomic
update(Ref, Value) ->
    true = ets:update_element(?ETS_NAME, Ref, {#future.result, Value}).
