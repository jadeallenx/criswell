-module(criswell_app).
-behaviour(application).

-include("criswell.hrl").

-export([
  start/2,
  stop/1
]).



-define(ETS_PARAMS, [named_table, public, {keypos, #future.ref}, {write_concurrency, true}, {read_concurrency, true}]).

start(_Type, _Args) ->
    _Tid = ets:new(?ETS_NAME, ?ETS_PARAMS),
    criswell_sup:start_link().

stop(_State) ->
    ok.
