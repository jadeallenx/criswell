% @doc This is a `simple-one-for-one' supervisor which spawns the manager
% process. The manager process spawns the worker process with a monitor.

-module(criswell_sup).
-behaviour(supervisor).

-include("criswell.hrl").

-export([
    start_link/0,
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
         #{ id       => criswell_mgr, %% ignored anyway
            start    => {criswell_mgr, start_link, []},
            restart  => temporary,
            shutdown => 1000,
            type     => worker,
            modules  => [criswell_mgr]
          }
    ],
    Flags = #{ strategy  => simple_one_for_one,
               intensity => 3,
               period    => 60
             },
    {ok, {Flags, Children}}.
