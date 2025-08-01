#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, conv2d/2, gradient/1, threshold/2, printmatrix/1, main/0]).

% Generated by Mochi transpiler v0.10.42 (8222b92481) on 2025-07-27 23:38 +0700


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

conv2d(Img, K) ->
    try
        H = length(Img),
        W = length(lists:nth(0 + 1, Img)),
        N = length(K),
        Half = (N div 2),
        Out = [],
        Y = 0,
        Fun_4 = fun Fun_4_loop(H, Half, Img, K, N, Out, W, Y) ->
    case (Y < H) of
        true ->
            Row = [],
            X = 0,
            Fun_3 = fun Fun_3_loop(H, Half, Img, K, N, Out, Row, W, X, Y) ->
    case (X < W) of
        true ->
            Sum = 0,
            J = 0,
            Fun_2 = fun Fun_2_loop(H, Half, Img, J, K, N, Out, Row, Sum, W, X, Y) ->
    case (J < N) of
        true ->
            I = 0,
            Fun = fun Fun_loop(H, Half, I, Img, J, K, N, Out, Row, Sum, W, X, Y) ->
    case (I < N) of
        true ->
            Yy = ((Y + J) - Half),
            case (Yy < 0) of
        true -> Yy_2 = 0,
            Yy_3 = Yy_2;
        _ -> Yy_3 = Yy
    end,
            case (Yy_3 >= H) of
        true -> Yy_4 = (H - 1),
            Yy_5 = Yy_4;
        _ -> Yy_5 = Yy_3
    end,
            Xx = ((X + I) - Half),
            case (Xx < 0) of
        true -> Xx_2 = 0,
            Xx_3 = Xx_2;
        _ -> Xx_3 = Xx
    end,
            case (Xx_3 >= W) of
        true -> Xx_4 = (W - 1),
            Xx_5 = Xx_4;
        _ -> Xx_5 = Xx_3
    end,
            Sum_2 = (Sum + (lists:nth(Xx_5 + 1, lists:nth(Yy_5 + 1, Img)) * lists:nth(I + 1, lists:nth(J + 1, K)))),
            I_2 = (I + 1),
            Fun_loop(H, Half, I_2, Img, J, K, N, Out, Row, Sum_2, W, X, Y);
        _ -> {H, Half, I, Img, J, K, N, Out, Row, Sum, W, X, Y}
    end
end,
{H, Half, I_2, Img, J, K, N, Out, Row, Sum_2, W, X, Y} = Fun(H, Half, I, Img, J, K, N, Out, Row, Sum, W, X, Y),
            J_2 = (J + 1),
            Fun_2_loop(H, Half, Img, J_2, K, N, Out, Row, Sum_2, W, X, Y);
        _ -> {H, Half, Img, J, K, N, Out, Row, Sum, W, X, Y}
    end
end,
{H, Half, Img, J_2, K, N, Out, Row, Sum_2, W, X, Y} = Fun_2(H, Half, Img, J, K, N, Out, Row, Sum, W, X, Y),
            Row_2 = lists:append(Row, [Sum_2]),
            X_2 = (X + 1),
            Fun_3_loop(H, Half, Img, K, N, Out, Row_2, W, X_2, Y);
        _ -> {H, Half, Img, K, N, Out, Row, W, X, Y}
    end
end,
{H, Half, Img, K, N, Out, Row_2, W, X_2, Y} = Fun_3(H, Half, Img, K, N, Out, Row, W, X, Y),
            Out_2 = lists:append(Out, [Row_2]),
            Y_2 = (Y + 1),
            Fun_4_loop(H, Half, Img, K, N, Out_2, W, Y_2);
        _ -> {H, Half, Img, K, N, Out, W, Y}
    end
end,
{H, Half, Img, K, N, Out_2, W, Y_2} = Fun_4(H, Half, Img, K, N, Out, W, Y),
        Out_2
    catch {return, Ret} -> Ret end.

gradient(Img_2) ->
    try
        Hx = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]],
        Hy = [[1, 2, 1], [0, 0, 0], [-1, -2, -1]],
        Gx = conv2d(Img_2, Hx),
        Gy = conv2d(Img_2, Hy),
        H_2 = length(Img_2),
        W_2 = length(lists:nth(0 + 1, Img_2)),
        Out_3 = [],
        Y_3 = 0,
        Fun_6 = fun Fun_6_loop(Gx, Gy, H_2, Hx, Hy, Img_2, Out_3, W_2, Y_3) ->
    case (Y_3 < H_2) of
        true ->
            Row_3 = [],
            X_3 = 0,
            Fun_5 = fun Fun_5_loop(Gx, Gy, H_2, Hx, Hy, Img_2, Out_3, Row_3, W_2, X_3, Y_3) ->
    case (X_3 < W_2) of
        true ->
            G = ((lists:nth(X_3 + 1, lists:nth(Y_3 + 1, Gx)) * lists:nth(X_3 + 1, lists:nth(Y_3 + 1, Gx))) + (lists:nth(X_3 + 1, lists:nth(Y_3 + 1, Gy)) * lists:nth(X_3 + 1, lists:nth(Y_3 + 1, Gy)))),
            Row_4 = lists:append(Row_3, [G]),
            X_4 = (X_3 + 1),
            Fun_5_loop(Gx, Gy, H_2, Hx, Hy, Img_2, Out_3, Row_4, W_2, X_4, Y_3);
        _ -> {Gx, Gy, H_2, Hx, Hy, Img_2, Out_3, Row_3, W_2, X_3, Y_3}
    end
end,
{Gx, Gy, H_2, Hx, Hy, Img_2, Out_3, Row_4, W_2, X_4, Y_3} = Fun_5(Gx, Gy, H_2, Hx, Hy, Img_2, Out_3, Row_3, W_2, X_3, Y_3),
            Out_4 = lists:append(Out_3, [Row_4]),
            Y_4 = (Y_3 + 1),
            Fun_6_loop(Gx, Gy, H_2, Hx, Hy, Img_2, Out_4, W_2, Y_4);
        _ -> {Gx, Gy, H_2, Hx, Hy, Img_2, Out_3, W_2, Y_3}
    end
end,
{Gx, Gy, H_2, Hx, Hy, Img_2, Out_4, W_2, Y_4} = Fun_6(Gx, Gy, H_2, Hx, Hy, Img_2, Out_3, W_2, Y_3),
        Out_4
    catch {return, Ret} -> Ret end.

threshold(G_2, T) ->
    try
        H_3 = length(G_2),
        W_3 = length(lists:nth(0 + 1, G_2)),
        Out_5 = [],
        Y_5 = 0,
        Fun_8 = fun Fun_8_loop(G_2, H_3, Out_5, T, W_3, Y_5) ->
    case (Y_5 < H_3) of
        true ->
            Row_5 = [],
            X_5 = 0,
            Fun_7 = fun Fun_7_loop(G_2, H_3, Out_5, Row_5, T, W_3, X_5, Y_5) ->
    case (X_5 < W_3) of
        true ->
            case (lists:nth(X_5 + 1, lists:nth(Y_5 + 1, G_2)) >= T) of
        true -> Row_6 = lists:append(Row_5, [1]),
            Row_8 = Row_6;
        _ -> Row_7 = lists:append(Row_5, [0]),
            Row_8 = Row_7
    end,
            X_6 = (X_5 + 1),
            Fun_7_loop(G_2, H_3, Out_5, Row_8, T, W_3, X_6, Y_5);
        _ -> {G_2, H_3, Out_5, Row_5, T, W_3, X_5, Y_5}
    end
end,
{G_2, H_3, Out_5, Row_8, T, W_3, X_6, Y_5} = Fun_7(G_2, H_3, Out_5, Row_5, T, W_3, X_5, Y_5),
            Out_6 = lists:append(Out_5, [Row_8]),
            Y_6 = (Y_5 + 1),
            Fun_8_loop(G_2, H_3, Out_6, T, W_3, Y_6);
        _ -> {G_2, H_3, Out_5, T, W_3, Y_5}
    end
end,
{G_2, H_3, Out_6, T, W_3, Y_6} = Fun_8(G_2, H_3, Out_5, T, W_3, Y_5),
        Out_6
    catch {return, Ret} -> Ret end.

printmatrix(M) ->
    try
        Y_7 = 0,
        Fun_10 = fun Fun_10_loop(M, Y_7) ->
    case (Y_7 < length(M)) of
        true ->
            Line = "",
            X_7 = 0,
            Fun_9 = fun Fun_9_loop(Line, M, X_7, Y_7) ->
    case (X_7 < length(lists:nth(0 + 1, M))) of
        true ->
            Line_2 = (Line ++ lists:flatten(io_lib:format("~p", [lists:nth(X_7 + 1, lists:nth(Y_7 + 1, M))]))),
            case (X_7 < (length(lists:nth(0 + 1, M)) - 1)) of
        true -> Line_3 = (Line_2 ++ " "),
            Line_4 = Line_3;
        _ -> Line_4 = Line_2
    end,
            X_8 = (X_7 + 1),
            Fun_9_loop(Line_4, M, X_8, Y_7);
        _ -> {Line, M, X_7, Y_7}
    end
end,
{Line_4, M, X_8, Y_7} = Fun_9(Line, M, X_7, Y_7),
            io:format("~ts~n", [Line_4]),
            Y_8 = (Y_7 + 1),
            Fun_10_loop(M, Y_8);
        _ -> {M, Y_7}
    end
end,
{M, Y_8} = Fun_10(M, Y_7),
        nil
    catch {return, Ret} -> Ret end.

main() ->
    try
        Img_3 = [[0, 0, 0, 0, 0], [0, 255, 255, 255, 0], [0, 255, 255, 255, 0], [0, 255, 255, 255, 0], [0, 0, 0, 0, 0]],
        G_3 = gradient(Img_3),
        Edges = threshold(G_3, (1020 * 1020)),
        printmatrix(Edges),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('PI', 3.141592653589793),
    main(),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
