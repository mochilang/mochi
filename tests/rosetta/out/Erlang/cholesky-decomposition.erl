% cholesky-decomposition.erl - generated from cholesky-decomposition.mochi

sqrtApprox(X) ->
    Guess0 = X,
    I0 = 0,
    (fun Loop0(Guess, I) -> case (I < 20) of true -> Guess1 = (((Guess + (X / Guess))) / 2), I1 = (I + 1), Loop0(Guess1, I1); _ -> ok end end(Guess0, I0)),
    Guess1.

cholesky(A) ->
    N = length(A),
    L0 = [],
    I2 = 0,
    (fun Loop2(I, L) -> case (I < N) of true -> Row0 = [], J0 = 0, (fun Loop1(Row, J) -> case (J < N) of true -> Row1 = Row ++ [0], J1 = (J + 1), Loop1(Row1, J1); _ -> ok end end(Row0, J0)), L1 = L ++ [Row1], I3 = (I + 1), Loop2(L1, I3); _ -> ok end end(I2, L0)),
    I4 = 0,
    (fun Loop5(I) -> case (I < N) of true -> J2 = 0, (fun Loop4(J) -> case (J =< I) of true -> Sum0 = mochi_get(J, lists:nth((I)+1, A)), K0 = 0, (fun Loop3(Sum, K) -> case (K < J) of true -> Sum1 = (Sum - (mochi_get(K, lists:nth((I)+1, L1)) * mochi_get(K, lists:nth((J)+1, L1)))), K1 = (K + 1), Loop3(Sum1, K1); _ -> ok end end(Sum0, K0)), (case (I == J) of true -> LInner0 = mochi_get(I, L1), LInnerUpd0 = maps:put(J, sqrtApprox(Sum1), LInner0), L2 = L1#{I => LInnerUpd0}; _ -> LInner1 = mochi_get(I, L2), LInnerUpd1 = maps:put(J, (Sum1 / mochi_get(J, mochi_get(J, L2))), LInner1), L3 = L2#{I => LInnerUpd1} end), J3 = (J + 1), Loop4(J3); _ -> ok end end(J2)), I5 = (I + 1), Loop5(I5); _ -> ok end end(I4)),
    L3.

printMat(M) ->
    I6 = 0,
    (fun Loop7(I) -> case (I < length(M)) of true -> Line0 = "", J4 = 0, (fun Loop6(Line, J) -> case (J < length(lists:nth((I)+1, M))) of true -> Line1 = (Line + lists:flatten(io_lib:format("~p", [mochi_get(J, lists:nth((I)+1, M))]))), (case (J < (length(lists:nth((I)+1, M)) - 1)) of true -> Line2 = Line ++ " "; _ -> ok end), J5 = (J + 1), Loop6(Line2, J5); _ -> ok end end(Line0, J4)), io:format("~p~n", [Line2]), I7 = (I + 1), Loop7(I7); _ -> ok end end(I6)).

demo(A) ->
    io:format("~p~n", ["A:"]),
    printMat(A),
    L = cholesky(A),
    io:format("~p~n", ["L:"]),
    printMat(L).

main(_) ->
    demo([[25, 15, -5], [15, 18, 0], [-5, 0, 11]]),
    demo([[18, 22, 54, 42], [22, 70, 86, 62], [54, 86, 174, 134], [42, 62, 134, 106]]).

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
