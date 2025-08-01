#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, fork/1]).

% Generated by Mochi transpiler v0.10.55 (67b72aa5ea) on 2025-08-02 22:23 +0700


mochi_now() ->
    case erlang:get(now_seed) of
        undefined ->
            case os:getenv("MOCHI_NOW_SEED") of
                false -> erlang:system_time(nanosecond);
                S ->
                    case catch list_to_integer(S) of
                        {'EXIT', _} -> erlang:system_time(nanosecond);
                        Seed ->
                            erlang:put(now_seed, Seed),
                            mochi_now()
                    end
            end;
        Seed ->
            Seed2 = (Seed * 1664525 + 1013904223) rem 2147483647,
            erlang:put(now_seed, Seed2),
            Seed2
    end.


mochi_not(X) ->
    case X of
        true -> false;
        false -> true;
        nil -> true;
        _ -> false
    end.

fork(HasChild) ->
    try
        Pid = erlang:get('nextPID'),
        erlang:put('nextPID', (erlang:get('nextPID') + 1)),
        io:format("~ts~n", [("PID: " ++ lists:flatten(io_lib:format("~p", [Pid])))]),
        case mochi_not(HasChild) of
        true -> io:format("~ts~n", ["Done."]),
            throw({return, nil});
        _ -> ok
    end,
        ChildPID = erlang:get('nextPID'),
        io:format("~ts~n", [("Child's PID: " ++ lists:flatten(io_lib:format("~p", [ChildPID])))]),
        fork(false),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('nextPID', 1),
    fork(true),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
