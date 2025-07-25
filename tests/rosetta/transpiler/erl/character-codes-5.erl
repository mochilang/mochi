#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, chr/1]).

% Generated by Mochi transpiler v0.10.41 (cccd67ba44) on 2025-07-26 23:59 +0700


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

chr(N) ->
    try
        case (N == 97) of
        true -> throw({return, "a"});
        _ -> ok
    end,
        case (N == 960) of
        true -> throw({return, "π"});
        _ -> ok
    end,
        case (N == 65) of
        true -> throw({return, "A"});
        _ -> ok
    end,
        "?"
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    io:format("~p~n", [chr(97)]),
    io:format("~p~n", [chr(960)]),
    io:format("~p~n", [(chr(97) ++ chr(960))]),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
