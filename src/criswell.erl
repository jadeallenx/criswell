%% @doc This is the main API for the criswell library.

-module(criswell).
-include("criswell.hrl").

-export([
    promise/1,
    promise/2,
    value/1,
    await/1
]).

-define(SLEEP_DELAY, 2).

%% @doc Return a promise for the value of `F' which can either be a function or
%% a MFA style tuple. Gets a default timeout value of 5000 milliseconds.
-spec promise( F :: mfa() | fun() ) -> Promise :: promise().
promise(F) ->
    promise(F, 5000).

%% @doc Return a promise for the value of `F' which can either be a function or
%% a MFA style tuple. The timeout value is in milliseconds.
-spec promise( F :: mfa() | fun(), Timeout :: pos_integer() ) -> Promise :: promise().
promise(F, Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
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
%% are `{error, not_found}' if the given promise is not in the system, or
%% `{error, timeout}' if the promise exceeded its timeout value.  Returns an
%% `{ok, Term}' tuple if successful. Term may be any arbitrary term. An attempt
%% is made to treat error tuples as error values, but it is possible an error
%% might be treated as an "ok" result.
-spec value( Promise :: promise() ) -> not_computed | 
                                       {error, not_found} | 
                                       {error, timeout} | 
                                       {error, Reason :: term()} | 
                                       {ok, Result :: term()}.
value({promise, Ref}) ->
    case ets:lookup(?ETS_NAME, Ref) of
         [] -> {error, not_found};
         [R] -> handle_result(R#future.result)
    end;
value(_Other) ->
    {error, bad_promise}.

%% @doc Wait for a promise to be fulfilled by an error or by a result.
%% This call may potentially block until the promise times out, although
%% there is a sleep call so the Erlang scheduler is hinted that it may 
%% run another job. 
%%
%% If you want a guaranteed non-blocking status check, use `value/1'.
-spec await( Promise :: promise() ) -> {error, Reason :: term()} | {ok, Result :: term()}.
await(Promise) ->
    Result = value(Promise),
    case Result of
         not_computed -> timer:sleep(?SLEEP_DELAY), await(Promise);
         {error, Reason} -> {error, Reason};
         V -> V
    end.

handle_result(timeout) -> {error, timeout};
handle_result(not_computed) -> not_computed;
handle_result({error, Reason}) -> {error, Reason};
handle_result(Value) -> {ok, Value}.
