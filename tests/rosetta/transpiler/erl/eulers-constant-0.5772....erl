#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, ln/1, harmonic/1, main/0]).

% Generated by Mochi transpiler v0.10.55 (6aa59f472e) on 2025-08-02 17:26 +0700


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

ln(X) ->
    try
        K = 0,
        V = X,
        Fun = fun Fun_loop(K, V, X) ->
    case (V >= 2) of
        true ->
            V_2 = (V / 2),
            K_2 = (K + 1),
            Fun_loop(K_2, V_2, X);
        _ -> {K, V, X}
    end
end,
{K_2, V_2, X} = Fun(K, V, X),
        Fun_2 = fun Fun_2_loop(K_2, V_2, X) ->
    case (V_2 < 1) of
        true ->
            V_3 = (V_2 * 2),
            K_3 = (K_2 - 1),
            Fun_2_loop(K_3, V_3, X);
        _ -> {K_2, V_2, X}
    end
end,
{K_3, V_3, X} = Fun_2(K_2, V_2, X),
        Z = ((V_3 - 1) / (V_3 + 1)),
        Zpow = Z,
        Sum = Z,
        I = 3,
        Fun_3 = fun Fun_3_loop(I, K_3, Sum, V_3, X, Z, Zpow) ->
    case (I =< 9) of
        true ->
            Zpow_2 = ((Zpow * Z) * Z),
            Sum_2 = (Sum + (Zpow_2 / float(I))),
            I_2 = (I + 2),
            Fun_3_loop(I_2, K_3, Sum_2, V_3, X, Z, Zpow_2);
        _ -> {I, K_3, Sum, V_3, X, Z, Zpow}
    end
end,
{I_2, K_3, Sum_2, V_3, X, Z, Zpow_2} = Fun_3(I, K_3, Sum, V_3, X, Z, Zpow),
        Ln2 = 0.6931471805599453,
        ((K_3 * 0.6931471805599453) + (2 * Sum_2))
    catch {return, Ret} -> Ret end.

harmonic(N) ->
    try
        Sum_3 = 0,
        I_3 = 1,
        Fun_4 = fun Fun_4_loop(I_3, N, Sum_3) ->
    case (I_3 =< N) of
        true ->
            Sum_4 = (Sum_3 + (1 / float(I_3))),
            I_4 = (I_3 + 1),
            Fun_4_loop(I_4, N, Sum_4);
        _ -> {I_3, N, Sum_3}
    end
end,
{I_4, N, Sum_4} = Fun_4(I_3, N, Sum_3),
        Sum_4
    catch {return, Ret} -> Ret end.

main() ->
    try
        N_2 = 100000,
        Gamma = (harmonic(100000) - ln(float(100000))),
        io:format("~ts~n", [lists:flatten(io_lib:format("~p", [Gamma]))]),
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
