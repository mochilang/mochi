#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1]).

% Generated by Mochi transpiler v0.10.47 (eaacde736f) on 2025-07-28 11:52 +0700


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

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('depth', 10),
    erlang:put('seq', "F"),
    erlang:put('i', 0),
    Fun_2 = fun Fun_2_loop() ->
    case (erlang:get('i') < 10) of
        true ->
            Rev = "",
            J = (length(erlang:get('seq')) - 1),
            Fun = fun Fun_loop(J, Rev) ->
    case (J >= 0) of
        true ->
            C = string:substr(erlang:get('seq'), J + 1, ((J + 1) - J)),
            case (C == "L") of
        true -> Rev_2 = (Rev ++ "R"),
            Rev_6 = Rev_2;
        _ -> case (C == "R") of
        true -> Rev_3 = (Rev ++ "L"),
            Rev_5 = Rev_3;
        _ -> Rev_4 = (Rev ++ C),
            Rev_5 = Rev_4
    end,
            Rev_6 = Rev_5
    end,
            J_2 = (J - 1),
            Fun_loop(J_2, Rev_6);
        _ -> {J, Rev}
    end
end,
{J_2, Rev_6} = Fun(J, Rev),
            erlang:put('seq', ((erlang:get('seq') ++ "L") ++ Rev_6)),
            erlang:put('i', (erlang:get('i') + 1)),
            Fun_2_loop();
        _ -> {}
    end
end,
{} = Fun_2(),
    io:format("~ts~n", [erlang:get('seq')]),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
