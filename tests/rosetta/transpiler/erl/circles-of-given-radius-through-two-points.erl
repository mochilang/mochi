#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, sqrtapprox/1, hypot/2, circles/3]).

% Generated by Mochi transpiler v0.10.41 (fc61869ba1) on 2025-07-27 00:43 +0700


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

sqrtapprox(X) ->
    try
        G = X,
        I = 0,
        Fun = fun Fun_loop(G, I, X) ->
    case (I < 40) of
        true ->
            G_2 = ((G + (X / G)) / 2),
            I_2 = (I + 1),
            Fun_loop(G_2, I_2, X);
        _ -> {G, I, X}
    end
end,
{G_2, I_2, X} = Fun(G, I, X),
        G_2
    catch {return, Ret} -> Ret end.

hypot(X_2, Y) ->
    try
        sqrtapprox(((X_2 * X_2) + (Y * Y)))
    catch {return, Ret} -> Ret end.

circles(P1, P2, R) ->
    try
        case ((maps:get("x", P1, nil) == maps:get("x", P2, nil)) andalso (maps:get("y", P1, nil) == maps:get("y", P2, nil))) of
        true -> case (R == 0) of
        true -> throw({return, [P1, P1, "Coincident points with r==0.0 describe a degenerate circle."]});
        _ -> ok
    end,
            throw({return, [P1, P2, "Coincident points describe an infinite number of circles."]});
        _ -> ok
    end,
        case (R == 0) of
        true -> throw({return, [P1, P2, "R==0.0 does not describe circles."]});
        _ -> ok
    end,
        Dx = (maps:get("x", P2, nil) - maps:get("x", P1, nil)),
        Dy = (maps:get("y", P2, nil) - maps:get("y", P1, nil)),
        Q = hypot(Dx, Dy),
        case (Q > (2 * R)) of
        true -> throw({return, [P1, P2, "Points too far apart to form circles."]});
        _ -> ok
    end,
        M = #{"x" => ((maps:get("x", P1, nil) + maps:get("x", P2, nil)) / 2), "y" => ((maps:get("y", P1, nil) + maps:get("y", P2, nil)) / 2)},
        case (Q == (2 * R)) of
        true -> throw({return, [M, M, "Points form a diameter and describe only a single circle."]});
        _ -> ok
    end,
        D = sqrtapprox(((R * R) - ((Q * Q) / 4))),
        Ox = ((D * Dx) / Q),
        Oy = ((D * Dy) / Q),
        [#{"x" => (maps:get("x", M, nil) - Oy), "y" => (maps:get("y", M, nil) + Ox)}, #{"x" => (maps:get("x", M, nil) + Oy), "y" => (maps:get("y", M, nil) - Ox)}, "Two circles."]
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('Two', "Two circles."),
    erlang:put('R0', "R==0.0 does not describe circles."),
    erlang:put('Co', "Coincident points describe an infinite number of circles."),
    erlang:put('CoR0', "Coincident points with r==0.0 describe a degenerate circle."),
    erlang:put('Diam', "Points form a diameter and describe only a single circle."),
    erlang:put('Far', "Points too far apart to form circles."),
    erlang:put('td', [[#{"x" => 0.1234, "y" => 0.9876}, #{"x" => 0.8765, "y" => 0.2345}, 2], [#{"x" => 0, "y" => 2}, #{"x" => 0, "y" => 0}, 1], [#{"x" => 0.1234, "y" => 0.9876}, #{"x" => 0.1234, "y" => 0.9876}, 2], [#{"x" => 0.1234, "y" => 0.9876}, #{"x" => 0.8765, "y" => 0.2345}, 0.5], [#{"x" => 0.1234, "y" => 0.9876}, #{"x" => 0.1234, "y" => 0.9876}, 0]]),
    Fun_2 = fun Fun_2_loop(List) ->
    case List of
        [] -> {};
        [Tc|Tc_rest] ->
            P1_2 = lists:nth(0 + 1, Tc),
            P2_2 = lists:nth(1 + 1, Tc),
            R_2 = lists:nth(2 + 1, Tc),
            io:format("~ts~n", [(((("p1:  {" ++ lists:flatten(io_lib:format("~p", [maps:get("x", P1_2, nil)]))) ++ " ") ++ lists:flatten(io_lib:format("~p", [maps:get("y", P1_2, nil)]))) ++ "}")]),
            io:format("~ts~n", [(((("p2:  {" ++ lists:flatten(io_lib:format("~p", [maps:get("x", P2_2, nil)]))) ++ " ") ++ lists:flatten(io_lib:format("~p", [maps:get("y", P2_2, nil)]))) ++ "}")]),
            io:format("~ts~n", [("r:  " ++ lists:flatten(io_lib:format("~p", [R_2])))]),
            Res = circles(P1_2, P2_2, R_2),
            C1 = lists:nth(0 + 1, Res),
            C2 = lists:nth(1 + 1, Res),
            CaseStr = lists:nth(2 + 1, Res),
            io:format("~ts~n", [("   " ++ CaseStr)]),
            case ((CaseStr == "Points form a diameter and describe only a single circle.") orelse (CaseStr == "Coincident points with r==0.0 describe a degenerate circle.")) of
        true -> io:format("~ts~n", [(((("   Center:  {" ++ lists:flatten(io_lib:format("~p", [maps:get("x", C1, nil)]))) ++ " ") ++ lists:flatten(io_lib:format("~p", [maps:get("y", C1, nil)]))) ++ "}")]);
        _ -> case (CaseStr == "Two circles.") of
        true -> io:format("~ts~n", [(((("   Center 1:  {" ++ lists:flatten(io_lib:format("~p", [maps:get("x", C1, nil)]))) ++ " ") ++ lists:flatten(io_lib:format("~p", [maps:get("y", C1, nil)]))) ++ "}")]),
            io:format("~ts~n", [(((("   Center 2:  {" ++ lists:flatten(io_lib:format("~p", [maps:get("x", C2, nil)]))) ++ " ") ++ lists:flatten(io_lib:format("~p", [maps:get("y", C2, nil)]))) ++ "}")]);
        _ -> ok
    end
    end,
            io:format("~ts~n", [""]),
            Fun_2_loop(Tc_rest)
    end
end,
{} = Fun_2(erlang:get('td')),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
