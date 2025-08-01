#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, sortfloat/1, ceilf/1, fivenum/1]).

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


mochi_to_int(V) ->
    case erlang:is_integer(V) of
        true -> V;
        _ -> case erlang:is_float(V) of
            true -> trunc(V);
            _ -> list_to_integer(V)
        end
    end.


mochi_safe_mul(A, B) ->
    try A * B catch _:_ -> 1.0e308 end.

mochi_safe_div(A, B) ->
    try A / B catch _:_ -> 0.0 end.

mochi_safe_fmod(A, B) ->
    try math:fmod(A, B) catch _:_ -> 0.0 end.

sortfloat(Xs) ->
    try
        Arr = Xs,
        N = length(Arr),
        I = 0,
        Fun_2 = fun Fun_2_loop(Arr, I, N, Xs) ->
    case (I < N) of
        true ->
            J = 0,
            Fun = fun Fun_loop(Arr, I, J, N, Xs) ->
    case (J < (N - 1)) of
        true ->
            case ((case erlang:is_map(Arr) of true -> maps:get(J, Arr, nil); _ -> lists:nth(J + 1, Arr) end) > (case erlang:is_map(Arr) of true -> maps:get((J + 1), Arr, nil); _ -> lists:nth((J + 1) + 1, Arr) end)) of
        true -> T = (case erlang:is_map(Arr) of true -> maps:get(J, Arr, nil); _ -> lists:nth(J + 1, Arr) end),
            Arr_2 = lists:sublist(Arr, J) ++ [(case erlang:is_map(Arr) of true -> maps:get((J + 1), Arr, nil); _ -> lists:nth((J + 1) + 1, Arr) end)] ++ lists:nthtail(J + 1, Arr),
            Arr_3 = lists:sublist(Arr_2, (J + 1)) ++ [T] ++ lists:nthtail((J + 1) + 1, Arr_2),
            Arr_4 = Arr_3,
            T_2 = T;
        _ -> Arr_4 = Arr,
            T_2 = nil
    end,
            J_2 = (J + 1),
            Fun_loop(Arr_4, I, J_2, N, Xs);
        _ -> {Arr, I, J, N, Xs}
    end
end,
{Arr_4, I, J_2, N, Xs} = Fun(Arr, I, J, N, Xs),
            I_2 = (I + 1),
            Fun_2_loop(Arr_4, I_2, N, Xs);
        _ -> {Arr, I, N, Xs}
    end
end,
{Arr_4, I_2, N, Xs} = Fun_2(Arr, I, N, Xs),
        Arr_4
    catch {return, Ret} -> Ret end.

ceilf(X) ->
    try
        I_3 = mochi_to_int(X),
        case (X > float(I_3)) of
        true -> throw({return, (I_3 + 1)});
        _ -> ok
    end,
        I_3
    catch {return, Ret} -> Ret end.

fivenum(A) ->
    try
        Arr_5 = sortfloat(A),
        N_2 = length(Arr_5),
        Half = ((N_2 + 3) - ((N_2 + 3) rem 2)),
        N4 = mochi_safe_div(float((Half div 2)), 2),
        Nf = float(N_2),
        D = [1, N4, mochi_safe_div((Nf + 1), 2), ((Nf + 1) - N4), Nf],
        Result = [],
        Idx = 0,
        Fun_3 = fun Fun_3_loop(A, Arr_5, D, Half, Idx, N_2, N4, Nf, Result) ->
    case (Idx < length(D)) of
        true ->
            De = (case erlang:is_map(D) of true -> maps:get(Idx, D, nil); _ -> lists:nth(Idx + 1, D) end),
            Fl = mochi_to_int((De - 1)),
            Cl = ceilf((De - 1)),
            Result_2 = lists:append(Result, [mochi_safe_mul(0.5, ((case erlang:is_map(Arr_5) of true -> maps:get(Fl, Arr_5, nil); _ -> lists:nth(Fl + 1, Arr_5) end) + (case erlang:is_map(Arr_5) of true -> maps:get(Cl, Arr_5, nil); _ -> lists:nth(Cl + 1, Arr_5) end)))]),
            Idx_2 = (Idx + 1),
            Fun_3_loop(A, Arr_5, D, Half, Idx_2, N_2, N4, Nf, Result_2);
        _ -> {A, Arr_5, D, Half, Idx, N_2, N4, Nf, Result}
    end
end,
{A, Arr_5, D, Half, Idx_2, N_2, N4, Nf, Result_2} = Fun_3(A, Arr_5, D, Half, Idx, N_2, N4, Nf, Result),
        Result_2
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('x1', [36, 40, 7, 39, 41, 15]),
    erlang:put('x2', [15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43]),
    erlang:put('x3', [0.14082834, 0.0974879, 1.73131507, 0.87636009, -1.95059594, 0.73438555, -0.03035726, 1.4667597, -0.74621349, -0.72588772, 0.6390516, 0.61501527, -0.9898378, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578]),
    io:format("~ts~n", [lists:flatten(io_lib:format("~p", [fivenum(erlang:get('x1'))]))]),
    io:format("~ts~n", [lists:flatten(io_lib:format("~p", [fivenum(erlang:get('x2'))]))]),
    io:format("~ts~n", [lists:flatten(io_lib:format("~p", [fivenum(erlang:get('x3'))]))]),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
