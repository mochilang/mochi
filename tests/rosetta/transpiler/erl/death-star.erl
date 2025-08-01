#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, sqrtapprox/1, powf/2, normalize/1, dot/2, hitsphere/3, main/0]).

% Generated by Mochi transpiler v0.10.42 (c8363ccb18) on 2025-07-28 04:38 UTC


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


mochi_not(X) ->
    case X of
        true -> false;
        false -> true;
        nil -> true;
        _ -> false
    end.

sqrtapprox(X) ->
    try
        case (X =< 0) of
        true -> throw({return, 0});
        _ -> ok
    end,
        Guess = X,
        I = 0,
        Fun = fun Fun_loop(Guess, I, X) ->
    case (I < 20) of
        true ->
            Guess_2 = ((Guess + (X / Guess)) / 2),
            I_2 = (I + 1),
            Fun_loop(Guess_2, I_2, X);
        _ -> {Guess, I, X}
    end
end,
{Guess_2, I_2, X} = Fun(Guess, I, X),
        Guess_2
    catch {return, Ret} -> Ret end.

powf(Base, Exp) ->
    try
        Result = 1,
        I_3 = 0,
        Fun_2 = fun Fun_2_loop(Base, Exp, I_3, Result) ->
    case (I_3 < Exp) of
        true ->
            Result_2 = (Result * Base),
            I_4 = (I_3 + 1),
            Fun_2_loop(Base, Exp, I_4, Result_2);
        _ -> {Base, Exp, I_3, Result}
    end
end,
{Base, Exp, I_4, Result_2} = Fun_2(Base, Exp, I_3, Result),
        Result_2
    catch {return, Ret} -> Ret end.

normalize(V) ->
    try
        Len = sqrtapprox((((maps:get("x", V, nil) * maps:get("x", V, nil)) + (maps:get("y", V, nil) * maps:get("y", V, nil))) + (maps:get("z", V, nil) * maps:get("z", V, nil)))),
        #{"x" => (maps:get("x", V, nil) / Len), "y" => (maps:get("y", V, nil) / Len), "z" => (maps:get("z", V, nil) / Len)}
    catch {return, Ret} -> Ret end.

dot(A, B) ->
    try
        D = (((maps:get("x", A, nil) * maps:get("x", B, nil)) + (maps:get("y", A, nil) * maps:get("y", B, nil))) + (maps:get("z", A, nil) * maps:get("z", B, nil))),
        case (D < 0) of
        true -> throw({return, -D});
        _ -> ok
    end,
        0
    catch {return, Ret} -> Ret end.

hitsphere(S, X_2, Y) ->
    try
        Dx = (X_2 - maps:get("cx", S, nil)),
        Dy = (Y - maps:get("cy", S, nil)),
        Zsq = ((maps:get("r", S, nil) * maps:get("r", S, nil)) - ((Dx * Dx) + (Dy * Dy))),
        case (Zsq < 0) of
        true -> throw({return, #{"hit" => false}});
        _ -> ok
    end,
        Z = sqrtapprox(Zsq),
        #{"hit" => true, "z1" => (maps:get("cz", S, nil) - Z), "z2" => (maps:get("cz", S, nil) + Z)}
    catch {return, Ret} -> Ret end.

main() ->
    try
        Shades = ".:!*oe&#%@",
        Light = normalize(#{"x" => -50, "y" => 30, "z" => 50}),
        Pos = #{"cx" => 20, "cy" => 20, "cz" => 0, "r" => 20},
        Neg = #{"cx" => 1, "cy" => 1, "cz" => -6, "r" => 20},
        Yi = 0,
        Fun_4 = fun Fun_4_loop(Light, Neg, Pos, Shades, Yi) ->
    case (Yi =< 40) of
        true ->
            try
                Y_2 = (float(Yi) + 0.5),
                Line = "",
                Xi = -20,
                Fun_3 = fun Fun_3_loop(Light, Line, Neg, Pos, Shades, Xi, Y_2, Yi) ->
    case (Xi =< 60) of
        true ->
            try
                X_3 = ((((float(Xi) - maps:get("cx", Pos, nil)) / 2) + 0.5) + maps:get("cx", Pos, nil)),
                Hb = hitsphere(Pos, X_3, Y_2),
                case mochi_not(maps:get("hit", Hb, nil)) of
        true -> Line_2 = (Line ++ " "),
            Xi_2 = (Xi + 1),
            throw({continue, Light, Line_2, Neg, Pos, Shades, Xi_2, Y_2, Yi}),
            Line_3 = Line_2,
            Xi_3 = Xi_2;
        _ -> Line_3 = Line,
            Xi_3 = Xi
    end,
                Zb1 = maps:get("z1", Hb, nil),
                Zb2 = maps:get("z2", Hb, nil),
                Hs = hitsphere(Neg, X_3, Y_2),
                HitRes = 1,
                case mochi_not(maps:get("hit", Hs, nil)) of
        true -> HitRes_2 = 1,
            HitRes_10 = HitRes_2;
        _ -> case (maps:get("z1", Hs, nil) > Zb1) of
        true -> HitRes_3 = 1,
            HitRes_9 = HitRes_3;
        _ -> case (maps:get("z2", Hs, nil) > Zb2) of
        true -> HitRes_4 = 0,
            HitRes_8 = HitRes_4;
        _ -> case (maps:get("z2", Hs, nil) > Zb1) of
        true -> HitRes_5 = 2,
            HitRes_7 = HitRes_5;
        _ -> HitRes_6 = 1,
            HitRes_7 = HitRes_6
    end,
            HitRes_8 = HitRes_7
    end,
            HitRes_9 = HitRes_8
    end,
            HitRes_10 = HitRes_9
    end,
                case (HitRes_10 == 0) of
        true -> Line_4 = (Line_3 ++ " "),
            Xi_4 = (Xi_3 + 1),
            throw({continue, Light, Line_4, Neg, Pos, Shades, Xi_4, Y_2, Yi}),
            Line_5 = Line_4,
            Xi_5 = Xi_4;
        _ -> Line_5 = Line_3,
            Xi_5 = Xi_3
    end,
                Vec = nil,
                case (HitRes_10 == 1) of
        true -> Vec_2 = #{"x" => (X_3 - maps:get("cx", Pos, nil)), "y" => (Y_2 - maps:get("cy", Pos, nil)), "z" => (Zb1 - maps:get("cz", Pos, nil))},
            Vec_4 = Vec_2;
        _ -> Vec_3 = #{"x" => (maps:get("cx", Neg, nil) - X_3), "y" => (maps:get("cy", Neg, nil) - Y_2), "z" => (maps:get("cz", Neg, nil) - maps:get("z2", Hs, nil))},
            Vec_4 = Vec_3
    end,
                Vec_5 = normalize(Vec_4),
                B_2 = (powf(dot(Light, Vec_5), 2) + 0.5),
                Intensity = mochi_to_int(((1 - B_2) * float(length(Shades)))),
                case (Intensity < 0) of
        true -> Intensity_2 = 0,
            Intensity_3 = Intensity_2;
        _ -> Intensity_3 = Intensity
    end,
                case (Intensity_3 >= length(Shades)) of
        true -> Intensity_4 = (length(Shades) - 1),
            Intensity_5 = Intensity_4;
        _ -> Intensity_5 = Intensity_3
    end,
                Line_6 = (Line_5 ++ string:substr(Shades, Intensity_5 + 1, ((Intensity_5 + 1) - Intensity_5))),
                Xi_6 = (Xi_5 + 1),
                Fun_3_loop(Light, Line_6, Neg, Pos, Shades, Xi_6, Y_2, Yi)
            catch
                {continue, C0, C1, C2, C3, C4, C5, C6, C7} -> Fun_3_loop(C0, C1, C2, C3, C4, C5, C6, C7);
                break -> {Light, Line, Neg, Pos, Shades, Xi, Y_2, Yi}
            end;
        _ -> {Light, Line, Neg, Pos, Shades, Xi, Y_2, Yi}
    end
end,
{Light, Line_6, Neg, Pos, Shades, Xi_6, Y_2, Yi} = Fun_3(Light, Line, Neg, Pos, Shades, Xi, Y_2, Yi),
                io:format("~ts~n", [Line_6]),
                Yi_2 = (Yi + 1),
                Fun_4_loop(Light, Neg, Pos, Shades, Yi_2)
            catch
                {continue, C0, C1, C2, C3, C4} -> Fun_4_loop(C0, C1, C2, C3, C4);
                break -> {Light, Neg, Pos, Shades, Yi}
            end;
        _ -> {Light, Neg, Pos, Shades, Yi}
    end
end,
{Light, Neg, Pos, Shades, Yi_2} = Fun_4(Light, Neg, Pos, Shades, Yi),
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
