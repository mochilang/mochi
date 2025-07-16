% blum-integer.erl - generated from blum-integer.mochi

isPrime(N) ->
    (case (N < 2) of true -> false; _ -> ok end),
    (case (rem(N, 2) == 0) of true -> (N == 2); _ -> ok end),
    (case (rem(N, 3) == 0) of true -> (N == 3); _ -> ok end),
    D0 = 5,
    (fun Loop0(D) -> case ((D * D) =< N) of true -> (case (rem(N, D) == 0) of true -> false; _ -> ok end), D1 = (D + 2), (case (rem(N, D) == 0) of true -> false; _ -> ok end), D2 = (D + 4), Loop0(D2); _ -> ok end end(D0)),
    true.

firstPrimeFactor(N) ->
    (case (N == 1) of true -> 1; _ -> ok end),
    (case (rem(N, 3) == 0) of true -> 3; _ -> ok end),
    (case (rem(N, 5) == 0) of true -> 5; _ -> ok end),
    Inc0 = [4, 2, 4, 2, 4, 6, 2, 6],
    K0 = 7,
    I0 = 0,
    (fun Loop1(K, I) -> case ((K * K) =< N) of true -> (case (rem(N, K) == 0) of true -> K; _ -> ok end), K1 = (K + lists:nth((I)+1, Inc0)), I1 = rem(((I + 1)), length(Inc0)), Loop1(K1, I1); _ -> ok end end(K0, I0)),
    N.

indexOf(S, Ch) ->
    I2 = 0,
    (fun Loop2(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I3 = (I + 1), Loop2(I3); _ -> ok end end(I2)),
    -1.

padLeft(N, Width) ->
    S0 = lists:flatten(io_lib:format("~p", [N])),
    (fun Loop3(S) -> case (length(S) < Width) of true -> S1 = " " ++ S, Loop3(S1); _ -> ok end end(S0)),
    S1.

formatFloat(F, Prec) ->
    S = lists:flatten(io_lib:format("~p", [F])),
    Idx = indexOf(S, "."),
    (case (Idx < 0) of true -> S; _ -> ok end),
    Need = ((Idx + 1) + Prec),
    (case (length(S) > Need) of true -> string:substr(S, (0)+1, (Need)-(0)); _ -> ok end),
    S.

main() ->
    Blum0 = [],
    Counts0 = [0, 0, 0, 0],
    Digits0 = [1, 3, 7, 9],
    I4 = 1,
    Bc0 = 0,
    (fun Loop6() -> case true of true -> P = firstPrimeFactor(I4), (case (rem(P, 4) == 3) of true -> Q = ((I4 / P)), (case (((Q /= P) andalso (rem(Q, 4) == 3)) andalso isPrime(Q)) of true -> (case (Bc0 < 50) of true -> Blum1 = Blum0 ++ [I4]; _ -> ok end), D = rem(I4, 10), (case (D == 1) of true -> Counts1 = lists:sublist(Counts0, 0) ++ [(lists:nth((0)+1, Counts0) + 1)] ++ lists:nthtail((0)+1, Counts0); _ -> (case (D == 3) of true -> Counts2 = lists:sublist(Counts1, 1) ++ [(lists:nth((1)+1, Counts1) + 1)] ++ lists:nthtail((1)+1, Counts1); _ -> (case (D == 7) of true -> Counts3 = lists:sublist(Counts2, 2) ++ [(lists:nth((2)+1, Counts2) + 1)] ++ lists:nthtail((2)+1, Counts2); _ -> (case (D == 9) of true -> Counts4 = lists:sublist(Counts3, 3) ++ [(lists:nth((3)+1, Counts3) + 1)] ++ lists:nthtail((3)+1, Counts3); _ -> ok end) end) end) end), Bc1 = (Bc0 + 1), (case (Bc1 == 50) of true -> io:format("~p~n", ["First 50 Blum integers:"]), Idx0 = 0, (fun Loop5() -> case (Idx < 50) of true -> Line0 = "", J0 = 0, (fun Loop4(Line, Idx, J) -> case (J < 10) of true -> Line1 = (Line + padLeft(lists:nth((Idx)+1, Blum1), 3)) ++ " ", Idx1 = (Idx + 1), J1 = (J + 1), Loop4(J1, Line1, Idx1); _ -> ok end end(Line0, Idx, J0)), io:format("~p~n", [string:substr(Line1, (0)+1, ((length(Line1) - 1))-(0))]), Loop5(); _ -> ok end end()), throw(break); _ -> ok end); _ -> ok end); _ -> ok end), (case (rem(I4, 5) == 3) of true -> I5 = (I4 + 4); _ -> I6 = (I5 + 2) end), Loop6(); _ -> ok end end()).

main(_) ->
    main().
