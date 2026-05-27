-module(mochi_async).
-export([async/1, await/1, await_all/1, await_any/1]).

%% async/1 spawns a new process to evaluate F() and returns a Ref
%% that the caller can await. Phase 11.0.
async(F) ->
    Ref = make_ref(),
    Parent = self(),
    spawn(fun() ->
        Result = F(),
        Parent ! {Ref, Result}
    end),
    Ref.

%% await/1 blocks until the spawned process sends back {Ref, Result}.
%% Phase 11.1.
await(Ref) ->
    receive
        {Ref, Result} -> Result
    end.

%% await_all/1 collects results from a list of futures in order.
%% Phase 11.2.
await_all(Refs) ->
    [await(R) || R <- Refs].

%% await_any/1 returns the first result from any of the futures.
%% Phase 11.2.
await_any(Refs) ->
    receive
        {Ref, Result} ->
            case lists:member(Ref, Refs) of
                true  -> Result;
                false -> await_any(Refs)
            end
    end.
