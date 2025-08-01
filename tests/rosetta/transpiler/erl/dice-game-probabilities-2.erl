#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, roll/2, beats/5]).

% Generated by Mochi transpiler v0.10.42 (7d4a9e25e1) on 2025-07-28 04:49 UTC


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

roll(NDice, NSides) ->
    try
        Sum = 0,
        I = 0,
        Fun = fun Fun_loop(I, NDice, NSides, Sum) ->
    case (I < NDice) of
        true ->
            Sum_2 = ((Sum + (mochi_now() rem NSides)) + 1),
            I_2 = (I + 1),
            Fun_loop(I_2, NDice, NSides, Sum_2);
        _ -> {I, NDice, NSides, Sum}
    end
end,
{I_2, NDice, NSides, Sum_2} = Fun(I, NDice, NSides, Sum),
        Sum_2
    catch {return, Ret} -> Ret end.

beats(N1, S1, N2, S2, Trials) ->
    try
        Wins = 0,
        I_3 = 0,
        Fun_2 = fun Fun_2_loop(I_3, N1, N2, S1, S2, Trials, Wins) ->
    case (I_3 < Trials) of
        true ->
            case (roll(N1, S1) > roll(N2, S2)) of
        true -> Wins_2 = (Wins + 1),
            Wins_3 = Wins_2;
        _ -> Wins_3 = Wins
    end,
            I_4 = (I_3 + 1),
            Fun_2_loop(I_4, N1, N2, S1, S2, Trials, Wins_3);
        _ -> {I_3, N1, N2, S1, S2, Trials, Wins}
    end
end,
{I_4, N1, N2, S1, S2, Trials, Wins_3} = Fun_2(I_3, N1, N2, S1, S2, Trials, Wins),
        (float(Wins_3) / float(Trials))
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    io:format("~ts~n", [lists:flatten(io_lib:format("~p", [beats(9, 4, 6, 6, 1000)]))]),
    io:format("~ts~n", [lists:flatten(io_lib:format("~p", [beats(5, 10, 7, 6, 1000)]))]),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
