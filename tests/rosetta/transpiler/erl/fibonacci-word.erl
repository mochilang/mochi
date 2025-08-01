#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, entropy/1, fibonacciword/1, main/0, pad/2, fmt/1, floorf/1, indexof/2]).

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


mochi_to_int(V) ->
    case erlang:is_integer(V) of
        true -> V;
        _ -> case erlang:is_float(V) of
            true -> trunc(V);
            _ -> list_to_integer(V)
        end
    end.


mochi_index_of(S, Ch) when is_list(S) ->
    Char = case Ch of
        [C|_] -> C;
        <<C,_/binary>> -> C;
        C when is_integer(C) -> C;
        _ -> $\0
    end,
    case string:chr(S, Char) of
        0 -> -1;
        N -> N - 1
    end;
mochi_index_of(_, _) -> -1.

entropy(S) ->
    try
        Counts = #{},
        I = 0,
        Fun = fun Fun_loop(Counts, I, S) ->
    case (I < length(S)) of
        true ->
            Ch = string:substr(S, I + 1, ((I + 1) - I)),
            case maps:is_key(Ch, Counts) of
        true -> Counts_2 = maps:put(Ch, (maps:get(Ch, Counts, nil) + 1), Counts),
            Counts_4 = Counts_2;
        _ -> Counts_3 = maps:put(Ch, 1, Counts),
            Counts_4 = Counts_3
    end,
            I_2 = (I + 1),
            Fun_loop(Counts_4, I_2, S);
        _ -> {Counts, I, S}
    end
end,
{Counts_4, I_2, S} = Fun(Counts, I, S),
        Hm = 0,
        Fun_2 = fun Fun_2_loop(List, Counts_4, Hm, I_2, S) ->
    case List of
        [] -> {Counts_4, Hm, I_2, S};
        [K|K_rest] ->
            C = float((case erlang:is_map(Counts_4) of true -> maps:get(K, Counts_4, nil); _ -> lists:nth(K + 1, Counts_4) end)),
            Hm_2 = (Hm + (C * (nil div nil))),
            Fun_2_loop(K_rest, Counts_4, Hm_2, I_2, S)
    end
end,
{Counts_4, Hm_2, I_2, S} = Fun_2(maps:keys(Counts_4), Counts_4, Hm, I_2, S),
        L = float(length(S)),
        ((nil div nil) - (Hm_2 / L))
    catch {return, Ret} -> Ret end.

fibonacciword(N) ->
    try
        A = "1",
        B = "0",
        I_3 = 1,
        Fun_3 = fun Fun_3_loop(A, B, I_3, N) ->
    case (I_3 < N) of
        true ->
            Tmp = B,
            B_2 = (B ++ A),
            A_2 = Tmp,
            I_4 = (I_3 + 1),
            Fun_3_loop(A_2, B_2, I_4, N);
        _ -> {A, B, I_3, N}
    end
end,
{A_2, B_2, I_4, N} = Fun_3(A, B, I_3, N),
        A_2
    catch {return, Ret} -> Ret end.

main() ->
    try
        io:format("~ts~n", [((pad("N", 3) ++ pad("Length", 9)) ++ "  Entropy      Word")]),
        N_2 = 1,
        Fun_4 = fun Fun_4_loop(N_2) ->
    case (N_2 < 10) of
        true ->
            S_2 = fibonacciword(N_2),
            io:format("~ts~n", [(((((pad(lists:flatten(io_lib:format("~p", [N_2])), 3) ++ pad(lists:flatten(io_lib:format("~p", [length(S_2)])), 9)) ++ "  ") ++ fmt(entropy(S_2))) ++ "  ") ++ S_2)]),
            N_3 = (N_2 + 1),
            Fun_4_loop(N_3);
        _ -> {N_2}
    end
end,
{N_3} = Fun_4(N_2),
        Fun_5 = fun Fun_5_loop(N_3) ->
    case (N_3 =< 37) of
        true ->
            S_3 = fibonacciword(N_3),
            io:format("~ts~n", [(((pad(lists:flatten(io_lib:format("~p", [N_3])), 3) ++ pad(lists:flatten(io_lib:format("~p", [length(S_3)])), 9)) ++ "  ") ++ fmt(entropy(S_3)))]),
            N_4 = (N_3 + 1),
            Fun_5_loop(N_4);
        _ -> {N_3}
    end
end,
{N_4} = Fun_5(N_3),
        nil
    catch {return, Ret} -> Ret end.

pad(S_4, W) ->
    try
        T = S_4,
        Fun_6 = fun Fun_6_loop(S_4, T, W) ->
    case (length(T) < W) of
        true ->
            T_2 = (" " ++ T),
            Fun_6_loop(S_4, T_2, W);
        _ -> {S_4, T, W}
    end
end,
{S_4, T_2, W} = Fun_6(S_4, T, W),
        T_2
    catch {return, Ret} -> Ret end.

fmt(X) ->
    try
        Y = (floorf(((X * 1.0e+08) + 0.5)) / 1.0e+08),
        S_5 = lists:flatten(io_lib:format("~p", [Y])),
        Dot = mochi_index_of(S_5, "."),
        case (Dot == (0 - 1)) of
        true -> S_6 = (S_5 ++ ".00000000"),
            D_3 = nil,
            Fun_8 = nil,
            S_8 = S_6;
        _ -> D = ((length(S_5) - Dot) - 1),
            Fun_7 = fun Fun_7_loop(D, Dot, S_5, X, Y) ->
    case (D < 8) of
        true ->
            S_7 = (S_5 ++ "0"),
            D_2 = (D + 1),
            Fun_7_loop(D_2, Dot, S_7, X, Y);
        _ -> {D, Dot, S_5, X, Y}
    end
end,
{D_2, Dot, S_7, X, Y} = Fun_7(D, Dot, S_5, X, Y),
            D_3 = D_2,
            Fun_8 = Fun_7,
            S_8 = S_7
    end,
        S_8
    catch {return, Ret} -> Ret end.

floorf(X_2) ->
    try
        Y_2 = mochi_to_int(X_2),
        float(Y_2)
    catch {return, Ret} -> Ret end.

indexof(S_9, Ch_2) ->
    try
        I_5 = 0,
        Fun_9 = fun Fun_9_loop(Ch_2, I_5, S_9) ->
    case (I_5 < length(S_9)) of
        true ->
            case (string:substr(S_9, I_5 + 1, ((I_5 + 1) - I_5)) == Ch_2) of
        true -> throw({return, I_5});
        _ -> ok
    end,
            I_6 = (I_5 + 1),
            Fun_9_loop(Ch_2, I_6, S_9);
        _ -> {Ch_2, I_5, S_9}
    end
end,
{Ch_2, I_6, S_9} = Fun_9(Ch_2, I_5, S_9),
        (0 - 1)
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
