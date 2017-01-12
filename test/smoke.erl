-module(smoke).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    F = fun() -> 2+2 end,
    ok = application:start(criswell),
    P0 = criswell:promise(F, 100),
    ?assertMatch({promise, _}, P0),
    TF = fun() -> criswell:value(P0) end,
    ?assert(await_value({ok,4}, TF, 10)),
    ok.

await_value(_Value, _Fun, 0) -> false;
await_value(Value, Fun, Retries) ->
    Candidate = Fun(),
    case Candidate of
	 timeout -> false;
         Value   -> true;
	 _Other  -> 
              timer:sleep(5),
	      await_value(Value, Fun, Retries-1)
    end.
