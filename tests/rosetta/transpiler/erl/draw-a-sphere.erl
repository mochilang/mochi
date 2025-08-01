#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, sqrtapprox/1, powf/2, normalize/1, dot/2, drawsphere/5, main/0]).

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


mochi_to_int(V) ->
    case erlang:is_integer(V) of
        true -> V;
        _ -> case erlang:is_float(V) of
            true -> trunc(V);
            _ -> list_to_integer(V)
        end
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

drawsphere(R, K, Ambient, Light, Shades) ->
    try
        I_5 = -R,
        Fun_4 = fun Fun_4_loop(Ambient, I_5, K, Light, R, Shades) ->
    case (I_5 =< R) of
        true ->
            X_2 = (float(I_5) + 0.5),
            Line = "",
            J = -(2 * R),
            Fun_3 = fun Fun_3_loop(Ambient, I_5, J, K, Light, Line, R, Shades, X_2) ->
    case (J =< (2 * R)) of
        true ->
            Y = ((float(J) / 2) + 0.5),
            case (((X_2 * X_2) + (Y * Y)) =< (float(R) * float(R))) of
        true -> Zsq = (((float(R) * float(R)) - (X_2 * X_2)) - (Y * Y)),
            Vec = normalize(#{"x" => X_2, "y" => Y, "z" => sqrtapprox(Zsq)}),
            B_2 = (powf(dot(Light, Vec), K) + Ambient),
            Intensity = mochi_to_int(((1 - B_2) * (float(length(Shades)) - 1))),
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
            Line_2 = (Line ++ string:substr(Shades, Intensity_5 + 1, ((Intensity_5 + 1) - Intensity_5))),
            B_3 = B_2,
            Intensity_6 = Intensity_5,
            Line_4 = Line_2,
            Vec_2 = Vec,
            Zsq_2 = Zsq;
        _ -> Line_3 = (Line ++ " "),
            B_3 = nil,
            Intensity_6 = nil,
            Line_4 = Line_3,
            Vec_2 = nil,
            Zsq_2 = nil
    end,
            J_2 = (J + 1),
            Fun_3_loop(Ambient, I_5, J_2, K, Light, Line_4, R, Shades, X_2);
        _ -> {Ambient, I_5, J, K, Light, Line, R, Shades, X_2}
    end
end,
{Ambient, I_5, J_2, K, Light, Line_4, R, Shades, X_2} = Fun_3(Ambient, I_5, J, K, Light, Line, R, Shades, X_2),
            io:format("~ts~n", [Line_4]),
            I_6 = (I_5 + 1),
            Fun_4_loop(Ambient, I_6, K, Light, R, Shades);
        _ -> {Ambient, I_5, K, Light, R, Shades}
    end
end,
{Ambient, I_6, K, Light, R, Shades} = Fun_4(Ambient, I_5, K, Light, R, Shades),
        nil
    catch {return, Ret} -> Ret end.

main() ->
    try
        Shades_2 = ".:!*oe&#%@",
        Light_2 = normalize(#{"x" => 30, "y" => 30, "z" => -50}),
        drawsphere(20, 4, 0.1, Light_2, ".:!*oe&#%@"),
        drawsphere(10, 2, 0.4, Light_2, ".:!*oe&#%@"),
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
