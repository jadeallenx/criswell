%% @doc This is the main API for the criswell library.

-module(criswell).
-include("criswell.hrl").

-export([
    promise/1,
    promise/2,
    value/1
]).

%% @doc Return a promise for the value of `F' which can either be a function or
%% a MFA style tuple. Gets a default timeout value of 5000 milliseconds.
-spec promise( F :: mfa() | fun() ) -> {ok, Ref :: reference()}.
promise(F) ->
    promise(F, 5000).

%% @doc Return a promise for the value of `F' which can either be a function or
%% a MFA style tuple. The timeout value is in milliseconds.
-spec promise( F :: mfa() | fun(), Timeout :: pos_integer() ) -> {ok, Ref :: reference()}.
promise(F, Timeout) ->
    Ref = make_ref(),
    R = #future{ ref = Ref, timeout = Timeout },
    true = ets:insert_new(?ETS_NAME, R),
    %% We need to insert the reference into the ETS table before we dispatch the function because
    %% the function might return super fast and if there's no reference, criswell_mgr:update/2 will
    %% blow up.
    {ok, Pid} = supervisor:start_child(criswell_sup, [Timeout, F, Ref]),
    true = ets:update_element(?ETS_NAME, Ref, {#future.pid, Pid}),
    {promise, Ref}.

%% @doc Get an associated value for the given promise. The value `not_computed'
%% is returned if the promise has not yet computed a value. Error conditions
%% are `not_found' if the given promise is not in the system, or `timeout' if
%% the promise exceeded its timeout value.  Returns an `{ok, Term}' tuple if
%% successful. Term may be any arbitrary term.
-spec value( Future :: {future, Ref :: reference()} ) -> not_found | not_computed | timeout | {ok, term()}.
value({promise, Ref}) ->
    case ets:lookup(?ETS_NAME, Ref) of
         [] -> not_found;
         [R] -> {ok, R#future.result}
    end.
