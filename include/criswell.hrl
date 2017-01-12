-define(ETS_NAME, '__criswell_state').

-record(future, {
    ref :: reference(),
    pid :: pid(),
    timeout = 5000 :: pos_integer(),
    result = not_computed :: term()
}).

