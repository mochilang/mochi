% bitmap-histogram.erl - generated from bitmap-histogram.mochi

image() ->
    [[0, 0, 10000], [65535, 65535, 65535], [65535, 65535, 65535]].

histogram(G, Bins) ->
    (case (Bins =< 0) of true -> Bins0 = length(lists:nth((0)+1, G)); _ -> ok end),
    H0 = [],
    I0 = 0,
    (fun Loop0(I, H) -> case (I < Bins0) of true -> H1 = H ++ [0], I1 = (I + 1), Loop0(H1, I1); _ -> ok end end(I0, H0)),
    Y0 = 0,
    (fun Loop2(Y) -> case (Y < length(G)) of true -> Row0 = lists:nth((Y)+1, G), X0 = 0, (fun Loop1(H, X) -> case (X < length(Row0)) of true -> P0 = mochi_get(X, Row0), Idx0 = ((((P0 * ((Bins0 - 1)))) / 65535)), H2 = lists:sublist(H, Idx0) ++ [(lists:nth((Idx0)+1, H) + 1)] ++ lists:nthtail((Idx0)+1, H), X1 = (X + 1), Loop1(H2, X1); _ -> ok end end(H1, X0)), Y1 = (Y + 1), Loop2(Y1); _ -> ok end end(Y0)),
    H2.

medianThreshold(H) ->
    Lb0 = 0,
    Ub0 = (length(H2) - 1),
    LSum0 = 0,
    USum0 = 0,
    (fun Loop3() -> case (Lb0 =< Ub0) of true -> (case ((LSum0 + lists:nth((Lb0)+1, H2)) < (USum0 + lists:nth((Ub0)+1, H2))) of true -> LSum1 = (LSum0 + lists:nth((Lb0)+1, H2)), Lb1 = (Lb0 + 1); _ -> USum1 = (USum0 + lists:nth((Ub0)+1, H2)), Ub1 = (Ub0 - 1) end), Loop3(); _ -> ok end end()),
    ((((Ub1 * 65535)) / length(H2))).

threshold(G, T) ->
    Out0 = [],
    Y2 = 0,
    (fun Loop5(Out, Y) -> case (Y < length(G)) of true -> Row1 = lists:nth((Y)+1, G), NewRow0 = [], X2 = 0, (fun Loop4(X) -> case (X < length(Row1)) of true -> (case (mochi_get(X, Row1) < T) of true -> NewRow1 = NewRow0 ++ [0]; _ -> NewRow2 = NewRow1 ++ [65535] end), X3 = (X + 1), Loop4(X3); _ -> ok end end(X2)), Out1 = Out ++ [NewRow2], Y3 = (Y + 1), Loop5(Out1, Y3); _ -> ok end end(Out0, Y2)),
    Out1.

printImage(G) ->
    Y4 = 0,
    (fun Loop7(Y) -> case (Y < length(G)) of true -> Row2 = lists:nth((Y)+1, G), Line0 = "", X4 = 0, (fun Loop6(X) -> case (X < length(Row2)) of true -> (case (mochi_get(X, Row2) == 0) of true -> Line1 = Line0 ++ "0"; _ -> Line2 = Line1 ++ "1" end), X5 = (X + 1), Loop6(X5); _ -> ok end end(X4)), io:format("~p~n", [Line2]), Y5 = (Y + 1), Loop7(Y5); _ -> ok end end(Y4)).

main() ->
    Img = image(),
    H = histogram(Img, 0),
    io:format("~p~n", ["Histogram: " ++ lists:flatten(io_lib:format("~p", [H]))]),
    T = medianThreshold(H),
    io:format("~p~n", ["Threshold: " ++ lists:flatten(io_lib:format("~p", [T]))]),
    Bw = threshold(Img, T),
    printImage(Bw).

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
