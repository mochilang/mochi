#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, primefactors/1, repeat/2, d/1, pad/1, main/0]).

% Generated by Mochi transpiler v0.10.41 (cac7e4e2bd) on 2025-07-26 17:59 +0700


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

primefactors(N) ->
    try
        Factors = [],
        X = N,
        Fun = fun Fun_loop(Factors, N, X) ->
    case ((X rem 2) == 0) of
        true ->
            Factors_2 = lists:append(Factors, [2]),
            X_2 = mochi_to_int((X div 2)),
            Fun_loop(Factors_2, N, X_2);
        _ -> {Factors, N, X}
    end
end,
{Factors_2, N, X_2} = Fun(Factors, N, X),
        P = 3,
        Fun_3 = fun Fun_3_loop(Factors_2, N, P, X_2) ->
    case ((P * P) =< X_2) of
        true ->
            Fun_2 = fun Fun_2_loop(Factors_2, N, P, X_2) ->
    case ((X_2 rem P) == 0) of
        true ->
            Factors_3 = lists:append(Factors_2, [P]),
            X_3 = mochi_to_int((X_2 div P)),
            Fun_2_loop(Factors_3, N, P, X_3);
        _ -> {Factors_2, N, P, X_2}
    end
end,
{Factors_3, N, P, X_3} = Fun_2(Factors_2, N, P, X_2),
            P_2 = (P + 2),
            Fun_3_loop(Factors_3, N, P_2, X_3);
        _ -> {Factors_2, N, P, X_2}
    end
end,
{Factors_3, N, P_2, X_3} = Fun_3(Factors_2, N, P, X_2),
        case (X_3 > 1) of
        true -> Factors_4 = lists:append(Factors_3, [X_3]),
            Factors_5 = Factors_4;
        _ -> Factors_5 = Factors_3
    end,
        Factors_5
    catch {return, Ret} -> Ret end.

repeat(Ch, N_2) ->
    try
        S = "",
        I = 0,
        Fun_4 = fun Fun_4_loop(Ch, I, N_2, S) ->
    case (I < N_2) of
        true ->
            S_2 = (S ++ Ch),
            I_2 = (I + 1),
            Fun_4_loop(Ch, I_2, N_2, S_2);
        _ -> {Ch, I, N_2, S}
    end
end,
{Ch, I_2, N_2, S_2} = Fun_4(Ch, I, N_2, S),
        S_2
    catch {return, Ret} -> Ret end.

d(N_3) ->
    try
        case (N_3 < 0) of
        true -> throw({return, -d(-N_3)});
        _ -> ok
    end,
        case (N_3 < 2) of
        true -> throw({return, 0});
        _ -> ok
    end,
        Factors_6 = [],
        case (N_3 < 1.0e+19) of
        true -> Factors_7 = primefactors(mochi_to_int(N_3)),
            Factors_13 = Factors_7,
            G_2 = nil;
        _ -> G = mochi_to_int((N_3 / 100)),
            Factors_8 = primefactors(G),
            Factors_9 = lists:append(Factors_8, [2]),
            Factors_10 = lists:append(Factors_9, [2]),
            Factors_11 = lists:append(Factors_10, [5]),
            Factors_12 = lists:append(Factors_11, [5]),
            Factors_13 = Factors_12,
            G_2 = G
    end,
        C = length(Factors_13),
        case (C == 1) of
        true -> throw({return, 1});
        _ -> ok
    end,
        case (C == 2) of
        true -> throw({return, float((lists:nth(0 + 1, Factors_13) + lists:nth(1 + 1, Factors_13)))});
        _ -> ok
    end,
        D = (N_3 / float(lists:nth(0 + 1, Factors_13))),
        ((d(D) * float(lists:nth(0 + 1, Factors_13))) + D)
    catch {return, Ret} -> Ret end.

pad(N_4) ->
    try
        S_3 = lists:flatten(io_lib:format("~p", [N_4])),
        Fun_5 = fun Fun_5_loop(N_4, S_3) ->
    case (length(S_3) < 4) of
        true ->
            S_4 = (" " ++ S_3),
            Fun_5_loop(N_4, S_4);
        _ -> {N_4, S_3}
    end
end,
{N_4, S_4} = Fun_5(N_4, S_3),
        S_4
    catch {return, Ret} -> Ret end.

main() ->
    try
        Vals = [],
        N_5 = -99,
        Fun_6 = fun Fun_6_loop(N_5, Vals) ->
    case (N_5 < 101) of
        true ->
            Vals_2 = lists:append(Vals, [mochi_to_int(d(float(N_5)))]),
            N_6 = (N_5 + 1),
            Fun_6_loop(N_6, Vals_2);
        _ -> {N_5, Vals}
    end
end,
{N_6, Vals_2} = Fun_6(N_5, Vals),
        I_3 = 0,
        Fun_8 = fun Fun_8_loop(I_3, N_6, Vals_2) ->
    case (I_3 < length(Vals_2)) of
        true ->
            Line = "",
            J = 0,
            Fun_7 = fun Fun_7_loop(I_3, J, Line, N_6, Vals_2) ->
    case (J < 10) of
        true ->
            Line_2 = (Line ++ pad(lists:nth((I_3 + J) + 1, Vals_2))),
            case (J < 9) of
        true -> Line_3 = (Line_2 ++ " "),
            Line_4 = Line_3;
        _ -> Line_4 = Line_2
    end,
            J_2 = (J + 1),
            Fun_7_loop(I_3, J_2, Line_4, N_6, Vals_2);
        _ -> {I_3, J, Line, N_6, Vals_2}
    end
end,
{I_3, J_2, Line_4, N_6, Vals_2} = Fun_7(I_3, J, Line, N_6, Vals_2),
            io:format("~ts~n", [Line_4]),
            I_4 = (I_3 + 10),
            Fun_8_loop(I_4, N_6, Vals_2);
        _ -> {I_3, N_6, Vals_2}
    end
end,
{I_4, N_6, Vals_2} = Fun_8(I_3, N_6, Vals_2),
        Pow = 1,
        M = 1,
        Fun_9 = fun Fun_9_loop(I_4, M, N_6, Pow, Vals_2) ->
    case (M < 21) of
        true ->
            Pow_2 = (Pow * 10),
            Exp = lists:flatten(io_lib:format("~p", [M])),
            case (length(Exp) < 2) of
        true -> Exp_2 = (Exp ++ " "),
            Exp_3 = Exp_2;
        _ -> Exp_3 = Exp
    end,
            Res = (lists:flatten(io_lib:format("~p", [M])) ++ repeat("0", (M - 1))),
            io:format("~ts~n", [((("D(10^" ++ Exp_3) ++ ") / 7 = ") ++ Res)]),
            M_2 = (M + 1),
            Fun_9_loop(I_4, M_2, N_6, Pow_2, Vals_2);
        _ -> {I_4, M, N_6, Pow, Vals_2}
    end
end,
{I_4, M_2, N_6, Pow_2, Vals_2} = Fun_9(I_4, M, N_6, Pow, Vals_2),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    main(),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = erlang:abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
