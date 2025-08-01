#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, sieve/1, goldbachcount/2, pad/1, main/0]).

% Generated by Mochi transpiler v0.10.55 (26b9f6045a) on 2025-08-02 23:10 +0700


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

sieve(Limit) ->
    try
        Primes = [],
        I = 0,
        Fun = fun Fun_loop(I, Limit, Primes) ->
    case (I < Limit) of
        true ->
            Primes_2 = lists:append(Primes, [true]),
            I_2 = (I + 1),
            Fun_loop(I_2, Limit, Primes_2);
        _ -> {I, Limit, Primes}
    end
end,
{I_2, Limit, Primes_2} = Fun(I, Limit, Primes),
        Primes_3 = lists:sublist(Primes_2, 0) ++ [false] ++ lists:nthtail(0 + 1, Primes_2),
        Primes_4 = lists:sublist(Primes_3, 1) ++ [false] ++ lists:nthtail(1 + 1, Primes_3),
        P = 2,
        Fun_4 = fun Fun_4_loop(I_2, Limit, P, Primes_4) ->
    case ((P * P) < Limit) of
        true ->
            case (case erlang:is_map(Primes_4) of true -> maps:get(P, Primes_4, nil); _ -> lists:nth(P + 1, Primes_4) end) of
        true -> K = (P * P),
            Fun_2 = fun Fun_2_loop(I_2, K, Limit, P, Primes_4) ->
    case (K < Limit) of
        true ->
            Primes_5 = lists:sublist(Primes_4, K) ++ [false] ++ lists:nthtail(K + 1, Primes_4),
            K_2 = (K + P),
            Fun_2_loop(I_2, K_2, Limit, P, Primes_5);
        _ -> {I_2, K, Limit, P, Primes_4}
    end
end,
{I_2, K_2, Limit, P, Primes_5} = Fun_2(I_2, K, Limit, P, Primes_4),
            Fun_3 = Fun_2,
            K_3 = K_2,
            Primes_6 = Primes_5;
        _ -> Fun_3 = Fun,
            K_3 = nil,
            Primes_6 = Primes_4
    end,
            P_2 = (P + 1),
            Fun_4_loop(I_2, Limit, P_2, Primes_6);
        _ -> {I_2, Limit, P, Primes_4}
    end
end,
{I_2, Limit, P_2, Primes_6} = Fun_4(I_2, Limit, P, Primes_4),
        Primes_6
    catch {return, Ret} -> Ret end.

goldbachcount(Primes_7, N) ->
    try
        C = 0,
        I_3 = 1,
        Fun_5 = fun Fun_5_loop(C, I_3, N, Primes_7) ->
    case (I_3 =< (N div 2)) of
        true ->
            case ((case erlang:is_map(Primes_7) of true -> maps:get(I_3, Primes_7, nil); _ -> lists:nth(I_3 + 1, Primes_7) end) andalso (case erlang:is_map(Primes_7) of true -> maps:get((N - I_3), Primes_7, nil); _ -> lists:nth((N - I_3) + 1, Primes_7) end)) of
        true -> C_2 = (C + 1),
            C_3 = C_2;
        _ -> C_3 = C
    end,
            I_4 = (I_3 + 1),
            Fun_5_loop(C_3, I_4, N, Primes_7);
        _ -> {C, I_3, N, Primes_7}
    end
end,
{C_3, I_4, N, Primes_7} = Fun_5(C, I_3, N, Primes_7),
        C_3
    catch {return, Ret} -> Ret end.

pad(N_2) ->
    try
        case (N_2 < 10) of
        true -> throw({return, ("  " ++ lists:flatten(io_lib:format("~p", [N_2])))});
        _ -> ok
    end,
        case (N_2 < 100) of
        true -> throw({return, (" " ++ lists:flatten(io_lib:format("~p", [N_2])))});
        _ -> ok
    end,
        lists:flatten(io_lib:format("~p", [N_2]))
    catch {return, Ret} -> Ret end.

main() ->
    try
        Primes_8 = sieve(1000),
        io:format("~ts~n", ["The first 100 Goldbach numbers:"]),
        Line = "",
        N_3 = 2,
        Count = 0,
        Fun_6 = fun Fun_6_loop(Count, Line, N_3, Primes_8) ->
    case (Count < 100) of
        true ->
            V = goldbachcount(Primes_8, (2 * N_3)),
            Line_2 = ((Line ++ pad(V)) ++ " "),
            Count_2 = (Count + 1),
            N_4 = (N_3 + 1),
            case ((Count_2 rem 10) == 0) of
        true -> io:format("~ts~n", [string:substr(Line_2, 0 + 1, ((length(Line_2) - 1) - 0))]),
            Line_3 = "",
            Line_4 = Line_3;
        _ -> Line_4 = Line_2
    end,
            Fun_6_loop(Count_2, Line_4, N_4, Primes_8);
        _ -> {Count, Line, N_3, Primes_8}
    end
end,
{Count_2, Line_4, N_4, Primes_8} = Fun_6(Count, Line, N_3, Primes_8),
        Val = goldbachcount(Primes_8, 1000),
        io:format("~ts~n", [("\nThe 1,000th Goldbach number = " ++ lists:flatten(io_lib:format("~p", [Val])))]),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    main(),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
