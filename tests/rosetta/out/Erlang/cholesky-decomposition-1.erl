% cholesky-decomposition-1.erl - generated from cholesky-decomposition-1.mochi

sqrtApprox(X) ->
    Guess0 = X,
    I0 = 0,
    (fun Loop0(Guess, I) -> case (I < 20) of true -> Guess1 = (((Guess + (X / Guess))) / 2), I1 = (I + 1), Loop0(Guess1, I1); _ -> ok end end(Guess0, I0)),
    Guess1.

makeSym(Order, Elements) ->
    #{"order" => Order, "ele" => Elements}.

unpackSym(M) ->
    N = mochi_get("order", M),
    Ele = mochi_get("ele", M),
    Mat0 = [],
    Idx0 = 0,
    R0 = 0,
    (fun Loop3(Mat, R) -> case (R < N) of true -> Row0 = [], C0 = 0, (fun Loop1(Idx, C, Row) -> case (C =< R) of true -> Row1 = Row ++ [mochi_get(Idx, Ele)], Idx1 = (Idx + 1), C1 = (C + 1), Loop1(Row1, Idx1, C1); _ -> ok end end(Idx0, C0, Row0)), (fun Loop2(Row, C) -> case (C < N) of true -> Row2 = Row ++ [0], C2 = (C + 1), Loop2(Row2, C2); _ -> ok end end(Row1, C1)), Mat1 = Mat ++ [Row2], R1 = (R + 1), Loop3(R1, Mat1); _ -> ok end end(Mat0, R0)),
    R2 = 0,
    (fun Loop5(R) -> case (R < N) of true -> C3 = (R + 1), (fun Loop4(Mat, C) -> case (C < N) of true -> MatInner0 = mochi_get(R, Mat), MatInnerUpd0 = maps:put(C, mochi_get(R, lists:nth((C)+1, Mat)), MatInner0), Mat2 = Mat#{R => MatInnerUpd0}, C4 = (C + 1), Loop4(Mat2, C4); _ -> ok end end(Mat1, C3)), R3 = (R + 1), Loop5(R3); _ -> ok end end(R2)),
    Mat2.

printMat(M) ->
    I2 = 0,
    (fun Loop7(I) -> case (I < length(M)) of true -> Line0 = "", J0 = 0, (fun Loop6(Line, J) -> case (J < length(lists:nth((I)+1, M))) of true -> Line1 = (Line + lists:flatten(io_lib:format("~p", [mochi_get(J, lists:nth((I)+1, M))]))), (case (J < (length(lists:nth((I)+1, M)) - 1)) of true -> Line2 = Line ++ " "; _ -> ok end), J1 = (J + 1), Loop6(Line2, J1); _ -> ok end end(Line0, J0)), io:format("~p~n", [Line2]), I3 = (I + 1), Loop7(I3); _ -> ok end end(I2)).

printSym(M) ->
    printMat(unpackSym(M)).

printLower(M) ->
    N = mochi_get("order", M),
    Ele = mochi_get("ele", M),
    Mat3 = [],
    Idx2 = 0,
    R4 = 0,
    (fun Loop10(Mat, R) -> case (R < N) of true -> Row3 = [], C5 = 0, (fun Loop8(Row, Idx, C) -> case (C =< R) of true -> Row4 = Row ++ [mochi_get(Idx, Ele)], Idx3 = (Idx + 1), C6 = (C + 1), Loop8(C6, Row4, Idx3); _ -> ok end end(Row3, Idx2, C5)), (fun Loop9(Row, C) -> case (C < N) of true -> Row5 = Row ++ [0], C7 = (C + 1), Loop9(Row5, C7); _ -> ok end end(Row4, C6)), Mat4 = Mat ++ [Row5], R5 = (R + 1), Loop10(Mat4, R5); _ -> ok end end(Mat3, R4)),
    printMat(Mat4).

choleskyLower(A) ->
    N = mochi_get("order", A),
    Ae = mochi_get("ele", A),
    Le0 = [],
    Idx4 = 0,
    (fun Loop11(Le, Idx) -> case (Idx < length(Ae)) of true -> Le1 = Le ++ [0], Idx5 = (Idx + 1), Loop11(Le1, Idx5); _ -> ok end end(Le0, Idx4)),
    Row6 = 1,
    Col0 = 1,
    Dr0 = 0,
    Dc0 = 0,
    I4 = 0,
    (fun Loop13(I) -> case (I < length(Ae)) of true -> E = mochi_get(I, Ae), (case (I < Dr0) of true -> D0 = (((E - lists:nth((I)+1, Le1))) / lists:nth((Dc0)+1, Le1)), Le2 = lists:sublist(Le1, I) ++ [D0] ++ lists:nthtail((I)+1, Le1), Ci0 = Col0, Cx0 = Dc0, J2 = (I + 1), (fun Loop12(Ci, Le, J, Cx) -> case (J =< Dr0) of true -> Cx1 = (Cx + Ci), Ci1 = (Ci + 1), Le3 = lists:sublist(Le, J) ++ [(lists:nth((J)+1, Le) + (D0 * lists:nth((Cx)+1, Le)))] ++ lists:nthtail((J)+1, Le), J3 = (J + 1), Loop12(Cx1, Ci1, Le3, J3); _ -> ok end end(Ci0, Le2, J2, Cx0)), Col1 = (Col0 + 1), Dc1 = (Dc0 + Col1); _ -> Le4 = lists:sublist(Le3, I) ++ [sqrtApprox((E - lists:nth((I)+1, Le3)))] ++ lists:nthtail((I)+1, Le3), Row7 = (Row6 + 1), Dr1 = (Dr0 + Row7), Col2 = 1, Dc2 = 0 end), I5 = (I + 1), Loop13(I5); _ -> ok end end(I4)),
    #{"order" => N, "ele" => Le4}.

demo(A) ->
    io:format("~p~n", ["A:"]),
    printSym(A),
    io:format("~p~n", ["L:"]),
    L = choleskyLower(A),
    printLower(L).

main(_) ->
    demo(makeSym(3, [25, 15, 18, -5, 0, 11])),
    demo(makeSym(4, [18, 22, 70, 54, 86, 174, 42, 62, 134, 106])).

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
