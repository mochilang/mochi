#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, randn/1, eqindices/1, main/0]).

% Generated by Mochi transpiler v0.10.54 (6b449c7962) on 2025-08-02 12:04 +0700


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

randn(N) ->
    try
        erlang:put('seed', (((erlang:get('seed') * 1664525) + 1013904223) rem 2147483647)),
        (erlang:get('seed') rem N)
    catch {return, Ret} -> Ret end.

eqindices(Xs) ->
    try
        R = 0,
        I = 0,
        Fun = fun Fun_loop(I, R, Xs) ->
    case (I < length(Xs)) of
        true ->
            R_2 = (R + (case erlang:is_map(Xs) of true -> maps:get(I, Xs, nil); _ -> lists:nth(I + 1, Xs) end)),
            I_2 = (I + 1),
            Fun_loop(I_2, R_2, Xs);
        _ -> {I, R, Xs}
    end
end,
{I_2, R_2, Xs} = Fun(I, R, Xs),
        L = 0,
        Eq = [],
        I_3 = 0,
        Fun_2 = fun Fun_2_loop(Eq, I_3, L, R_2, Xs) ->
    case (I_3 < length(Xs)) of
        true ->
            R_3 = (R_2 - (case erlang:is_map(Xs) of true -> maps:get(I_3, Xs, nil); _ -> lists:nth(I_3 + 1, Xs) end)),
            case (L == R_3) of
        true -> Eq_2 = lists:append(Eq, [I_3]),
            Eq_3 = Eq_2;
        _ -> Eq_3 = Eq
    end,
            L_2 = (L + (case erlang:is_map(Xs) of true -> maps:get(I_3, Xs, nil); _ -> lists:nth(I_3 + 1, Xs) end)),
            I_4 = (I_3 + 1),
            Fun_2_loop(Eq_3, I_4, L_2, R_3, Xs);
        _ -> {Eq, I_3, L, R_2, Xs}
    end
end,
{Eq_3, I_4, L_2, R_3, Xs} = Fun_2(Eq, I_3, L, R_2, Xs),
        Eq_3
    catch {return, Ret} -> Ret end.

main() ->
    try
        io:format("~p~n", [eqindices([-7, 1, 5, 2, -4, 3, 0])]),
        Verylong = [],
        I_5 = 0,
        Fun_3 = fun Fun_3_loop(I_5, Verylong) ->
    case (I_5 < 10000) of
        true ->
            erlang:put('seed', (((erlang:get('seed') * 1664525) + 1013904223) rem 2147483647)),
            Verylong_2 = lists:append(Verylong, [((erlang:get('seed') rem 1001) - 500)]),
            I_6 = (I_5 + 1),
            Fun_3_loop(I_6, Verylong_2);
        _ -> {I_5, Verylong}
    end
end,
{I_6, Verylong_2} = Fun_3(I_5, Verylong),
        io:format("~p~n", [eqindices(Verylong_2)]),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('seed', (mochi_now() rem 2147483647)),
    main(),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
