#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, floorf/1, indexof/2, fmt8/1, pad2/1, main/0, pow_int/2]).

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

floorf(X) ->
    try
        Y = mochi_to_int(X),
        float(Y)
    catch {return, Ret} -> Ret end.

indexof(S, Ch) ->
    try
        I = 0,
        Fun = fun Fun_loop(Ch, I, S) ->
    case (I < length(S)) of
        true ->
            case (string:substr(S, I + 1, ((I + 1) - I)) == Ch) of
        true -> throw({return, I});
        _ -> ok
    end,
            I_2 = (I + 1),
            Fun_loop(Ch, I_2, S);
        _ -> {Ch, I, S}
    end
end,
{Ch, I_2, S} = Fun(Ch, I, S),
        (0 - 1)
    catch {return, Ret} -> Ret end.

fmt8(X_2) ->
    try
        Y_2 = (floorf(((X_2 * 1.0e+08) + 0.5)) / 1.0e+08),
        S_2 = lists:flatten(io_lib:format("~p", [Y_2])),
        Dot = mochi_index_of(S_2, "."),
        case (Dot == (0 - 1)) of
        true -> S_3 = (S_2 ++ ".00000000"),
            Decs_3 = nil,
            Fun_3 = nil,
            S_5 = S_3;
        _ -> Decs = ((length(S_2) - Dot) - 1),
            Fun_2 = fun Fun_2_loop(Decs, Dot, S_2, X_2, Y_2) ->
    case (Decs < 8) of
        true ->
            S_4 = (S_2 ++ "0"),
            Decs_2 = (Decs + 1),
            Fun_2_loop(Decs_2, Dot, S_4, X_2, Y_2);
        _ -> {Decs, Dot, S_2, X_2, Y_2}
    end
end,
{Decs_2, Dot, S_4, X_2, Y_2} = Fun_2(Decs, Dot, S_2, X_2, Y_2),
            Decs_3 = Decs_2,
            Fun_3 = Fun_2,
            S_5 = S_4
    end,
        S_5
    catch {return, Ret} -> Ret end.

pad2(X_3) ->
    try
        S_6 = lists:flatten(io_lib:format("~p", [X_3])),
        case (length(S_6) < 2) of
        true -> S_7 = (" " ++ S_6),
            S_8 = S_7;
        _ -> S_8 = S_6
    end,
        S_8
    catch {return, Ret} -> Ret end.

main() ->
    try
        MaxIt = 13,
        MaxItJ = 10,
        A1 = 1,
        A2 = 0,
        D1 = 3.2,
        io:format("~ts~n", [" i       d"]),
        I_3 = 2,
        Fun_6 = fun Fun_6_loop(A1, A2, D1, I_3, MaxIt, MaxItJ) ->
    case (I_3 =< MaxIt) of
        true ->
            A = (A1 + ((A1 - A2) / D1)),
            J = 1,
            Fun_5 = fun Fun_5_loop(A, A1, A2, D1, I_3, J, MaxIt, MaxItJ) ->
    case (J =< MaxItJ) of
        true ->
            X_4 = 0,
            Y_3 = 0,
            K = 1,
            Limit = pow_int(2, I_3),
            Fun_4 = fun Fun_4_loop(A, A1, A2, D1, I_3, J, K, Limit, MaxIt, MaxItJ, X_4, Y_3) ->
    case (K =< Limit) of
        true ->
            Y_4 = (1 - ((2 * Y_3) * X_4)),
            X_5 = (A - (X_4 * X_4)),
            K_2 = (K + 1),
            Fun_4_loop(A, A1, A2, D1, I_3, J, K_2, Limit, MaxIt, MaxItJ, X_5, Y_4);
        _ -> {A, A1, A2, D1, I_3, J, K, Limit, MaxIt, MaxItJ, X_4, Y_3}
    end
end,
{A, A1, A2, D1, I_3, J, K_2, Limit, MaxIt, MaxItJ, X_5, Y_4} = Fun_4(A, A1, A2, D1, I_3, J, K, Limit, MaxIt, MaxItJ, X_4, Y_3),
            A_2 = (A - (X_5 div Y_4)),
            J_2 = (J + 1),
            Fun_5_loop(A_2, A1, A2, D1, I_3, J_2, MaxIt, MaxItJ);
        _ -> {A, A1, A2, D1, I_3, J, MaxIt, MaxItJ}
    end
end,
{A_2, A1, A2, D1, I_3, J_2, MaxIt, MaxItJ} = Fun_5(A, A1, A2, D1, I_3, J, MaxIt, MaxItJ),
            D = ((A1 - A2) / (A_2 - A1)),
            io:format("~ts~n", [((pad2(I_3) ++ "    ") ++ fmt8(D))]),
            D1_2 = D,
            A2_2 = A1,
            A1_2 = A_2,
            I_4 = (I_3 + 1),
            Fun_6_loop(A1_2, A2_2, D1_2, I_4, MaxIt, MaxItJ);
        _ -> {A1, A2, D1, I_3, MaxIt, MaxItJ}
    end
end,
{A1_2, A2_2, D1_2, I_4, MaxIt, MaxItJ} = Fun_6(A1, A2, D1, I_3, MaxIt, MaxItJ),
        nil
    catch {return, Ret} -> Ret end.

pow_int(Base, Exp) ->
    try
        R = 1,
        B = Base,
        E = Exp,
        Fun_7 = fun Fun_7_loop(B, Base, E, Exp, R) ->
    case (E > 0) of
        true ->
            case ((E rem 2) == 1) of
        true -> R_2 = (R * B),
            R_3 = R_2;
        _ -> R_3 = R
    end,
            B_2 = (B * B),
            E_2 = mochi_to_int((E div 2)),
            Fun_7_loop(B_2, Base, E_2, Exp, R_3);
        _ -> {B, Base, E, Exp, R}
    end
end,
{B_2, Base, E_2, Exp, R_3} = Fun_7(B, Base, E, Exp, R),
        R_3
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
