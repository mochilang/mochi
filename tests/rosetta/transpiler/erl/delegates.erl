#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, operation/1, newdelegate/0]).

% Generated by Mochi transpiler v0.10.42 (87b8f50ede) on 2025-07-28 04:40 UTC


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

operation(D) ->
    try
        (case maps:is_key("thing", maps:get("delegate", D, nil)) of
    true -> (maps:get("thing", maps:get("delegate", D, nil), nil))();
    _ -> "default implementation"
end)
    catch {return, Ret} -> Ret end.

newdelegate() ->
    try
        M = #{},
        M_2 = maps:put("thing", fun() ->
    try
        "delegate implementation"
    catch {return, Ret} -> Ret end
end, M),
        M_2
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('a', #{"delegate" => #{}}),
    io:format("~ts~n", [operation(erlang:get('a'))]),
    A = erlang:get('a'),
    A_2 = maps:put("delegate", #{}, A),
    erlang:put('a', A_2),
    io:format("~ts~n", [operation(erlang:get('a'))]),
    A_3 = erlang:get('a'),
    A_4 = maps:put("delegate", newdelegate(), A_3),
    erlang:put('a', A_4),
    io:format("~ts~n", [operation(erlang:get('a'))]),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
