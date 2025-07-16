% canny-edge-detector.erl - generated from canny-edge-detector.mochi

conv2d(Img, K) ->
    H = length(Img),
    W = length(lists:nth((0)+1, Img)),
    N = length(K),
    Half = (N / 2),
    Out0 = [],
    Y0 = 0,
    (fun Loop3(Out, Y) -> case (Y < H) of true -> Row0 = [], X0 = 0, (fun Loop2(Row, X) -> case (X < W) of true -> Sum0 = 0, J0 = 0, (fun Loop1(J) -> case (J < N) of true -> I0 = 0, (fun Loop0(Sum, I) -> case (I < N) of true -> Yy0 = ((Y + J) - Half), (case (Yy0 < 0) of true -> Yy1 = 0; _ -> ok end), (case (Yy1 >= H) of true -> Yy2 = (H - 1); _ -> ok end), Xx0 = ((X + I) - Half), (case (Xx0 < 0) of true -> Xx1 = 0; _ -> ok end), (case (Xx1 >= W) of true -> Xx2 = (W - 1); _ -> ok end), Sum1 = (Sum + (mochi_get(Xx2, lists:nth((Yy2)+1, Img)) * mochi_get(I, lists:nth((J)+1, K)))), I1 = (I + 1), Loop0(I1, Sum1); _ -> ok end end(Sum0, I0)), J1 = (J + 1), Loop1(J1); _ -> ok end end(J0)), Row1 = Row ++ [Sum1], X1 = (X + 1), Loop2(Row1, X1); _ -> ok end end(Row0, X0)), Out1 = Out ++ [Row1], Y1 = (Y + 1), Loop3(Out1, Y1); _ -> ok end end(Out0, Y0)),
    Out1.

gradient(Img) ->
    Hx = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]],
    Hy = [[1, 2, 1], [0, 0, 0], [-1, -2, -1]],
    Gx0 = conv2d(Img, Hx),
    Gy0 = conv2d(Img, Hy),
    H0 = length(Img),
    W0 = length(lists:nth((0)+1, Img)),
    Out2 = [],
    Y2 = 0,
    (fun Loop5(Out, Y) -> case (Y < H) of true -> Row2 = [], X2 = 0, (fun Loop4(Row, X) -> case (X < W) of true -> G = ((mochi_get(X, mochi_get(Y, Gx0)) * mochi_get(X, mochi_get(Y, Gx0))) + (mochi_get(X, mochi_get(Y, Gy0)) * mochi_get(X, mochi_get(Y, Gy0)))), Row3 = Row ++ [G], X3 = (X + 1), Loop4(Row3, X3); _ -> ok end end(Row2, X2)), Out3 = Out ++ [Row3], Y3 = (Y + 1), Loop5(Out3, Y3); _ -> ok end end(Out2, Y2)),
    Out3.

threshold(G, T) ->
    H1 = length(G),
    W1 = length(lists:nth((0)+1, G)),
    Out4 = [],
    Y4 = 0,
    (fun Loop7(Out, Y) -> case (Y < H) of true -> Row4 = [], X4 = 0, (fun Loop6(X) -> case (X < W) of true -> (case (mochi_get(X, lists:nth((Y)+1, G)) >= T) of true -> Row5 = Row4 ++ [1]; _ -> Row6 = Row5 ++ [0] end), X5 = (X + 1), Loop6(X5); _ -> ok end end(X4)), Out5 = Out ++ [Row6], Y5 = (Y + 1), Loop7(Y5, Out5); _ -> ok end end(Out4, Y4)),
    Out5.

printMatrix(M) ->
    Y6 = 0,
    (fun Loop9(Y) -> case (Y < length(M)) of true -> Line0 = "", X6 = 0, (fun Loop8(Line, X) -> case (X < length(lists:nth((0)+1, M))) of true -> Line1 = (Line + lists:flatten(io_lib:format("~p", [mochi_get(X, lists:nth((Y)+1, M))]))), (case (X < (length(lists:nth((0)+1, M)) - 1)) of true -> Line2 = Line ++ " "; _ -> ok end), X7 = (X + 1), Loop8(Line2, X7); _ -> ok end end(Line0, X6)), io:format("~p~n", [Line2]), Y7 = (Y + 1), Loop9(Y7); _ -> ok end end(Y6)).

main() ->
    Img = [[0, 0, 0, 0, 0], [0, 255, 255, 255, 0], [0, 255, 255, 255, 0], [0, 255, 255, 255, 0], [0, 0, 0, 0, 0]],
    G = gradient(Img),
    Edges = threshold(G, (1020 * 1020)),
    printMatrix(Edges).

main(_) ->
    main().

mochi_get(K, M) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error ->
            Name = atom_to_list(K),
            case string:tokens(Name, "_") of
                [Pref|_] ->
                    P = list_to_atom(Pref),
                    case maps:find(P, M) of
                        {ok, Sub} when is_map(Sub) -> maps:get(K, Sub, undefined);
                        _ -> undefined
                    end;
                _ -> undefined
            end
        end.
