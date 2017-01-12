Criswell
========
> Greetings, my friend. We are all interested in the future, for that is where
> you and I are going to spend the rest of our lives. And remember, my friend,
> future events such as these will affect you in the future. You are
> interested in the unknown, the mysterious, the unexplainable. That is
> why you are here. 

[Plan 9 from Outer Space](https://en.wikiquote.org/wiki/Plan_9_from_Outer_Space)

Criswell is a library for Erlang that implements the idea of
[futures](https://en.wikipedia.org/wiki/Futures_and_promises).  A future is a
promise that at some point in the (hopefully) near future, a result will be
available to a potentially long-running operation.

Build
-----
This library is written for and tested against OTP 19+. It may or may not work
with earlier OTP releases.

     rebar3 compile

API
---
The API is very simple. All of the functions are in the `criswell` module. 

### promise/1 ###
This function takes a single argument, a function or an MFA tuple. It gets a
default timeout value of 5000 milliseconds.

### promise/2 ###
This function takes two arguments. The first is a function or MFA tuple. The 
second is a timeout value.

Both promise functions return a tuple which you should treat as an opaque data
structure.

### value/1 ###
This function takes a promise tuple and gets the associated value if it exists
yet.  On success an `{ok, Term}` tuple will be returned. Errors might include:
`{error, not_found}`, `{error, timeout}` if the timeout value is exceeded, or
an arbitrary `{error, Reason}` tuple. 

Results should be returned with an `{ok, Result}` result tuple. 

**NOTE**: Although we attempt to deal with error tuples separately, it's
possible that an error might be returned inside of a result tuple.

### await/1 ###
This function waits for a promise to be fulfilled, either by an error tuple or
a result tuple.  Although the Erlang scheduler is hinted that it can start
another job, it's possible this call may cause a schedule to block until the
promise times out or returns a value.

If you want a guaranteed non-blocking value check, use `value/1`.

Example
-------
```
$ rebar3 shell
===> Verifying dependencies...
===> Compiling criswell
===> Getting log of git dependency failed in /Users/mallen/github/mrallen1/criswell. Falling back to version 0.0.0
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false] [dtrace]
Eshell V8.1  (abort with ^G)
```
```erlang
1> application:start(criswell).
ok
2> F = fun() -> 2+2 end.
#Fun<erl_eval.20.52032458>
3> P0 = criswell:promise(F).
{promise,#Ref<0.0.2.28>}
4> criswell:value(P0).
{ok,4}
```

#### Library Name ####
This library is named for the [The Amazing
Criswell](https://en.wikipedia.org/wiki/The_Amazing_Criswell) who appears in
several Ed Wood movies.  The ridiculous epigram at the beginning of this file
is part of the introduction to the movie Plan 9 from Outer Space in which
Criswell played a large part.
