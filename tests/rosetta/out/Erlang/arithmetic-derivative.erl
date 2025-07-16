% arithmetic-derivative.erl - generated from arithmetic-derivative.mochi

primeFactors(N) ->
    Factors0 = [],
    X0 = N,
    (fun Loop0(Factors, X) -> case (rem(X, 2) == 0) of true -> Factors1 = Factors ++ [2], X1 = ((X / 2)), Loop0(Factors1, X1); _ -> ok end end(Factors0, X0)),
    P0 = 3,
    (fun Loop2(P) -> case ((P * P) =< X1) of true -> (fun Loop1(Factors, X) -> case (rem(X, P) == 0) of true -> Factors2 = Factors ++ [P], X2 = ((X / P)), Loop1(Factors2, X2); _ -> ok end end(Factors1, X1)), P1 = (P + 2), Loop2(P1); _ -> ok end end(P0)),
    (case (X2 > 1) of true -> Factors3 = Factors2 ++ [X2]; _ -> ok end),
    Factors3.

repeat(Ch, N) ->
    S0 = "",
    I0 = 0,
    (fun Loop3(I, S) -> case (I < N) of true -> S1 = (S + Ch), I1 = (I + 1), Loop3(S1, I1); _ -> ok end end(I0, S0)),
    S1.

D(N) ->
    (case (N < 0) of true -> -D(-N); _ -> ok end),
    (case (N < 2) of true -> 0; _ -> ok end),
    Factors4 = [],
    (case (N < 1e+19) of true -> Factors5 = primeFactors((N)); _ -> G = ((N / 100)), Factors6 = primeFactors(G), Factors7 = Factors6 ++ [2], Factors8 = Factors7 ++ [2], Factors9 = Factors8 ++ [5], Factors10 = Factors9 ++ [5] end),
    C = length(Factors10),
    (case (C == 1) of true -> 1; _ -> ok end),
    (case (C == 2) of true -> ((lists:nth((0)+1, Factors10) + lists:nth((1)+1, Factors10))); _ -> ok end),
    D = (N / (lists:nth((0)+1, Factors10))),
    ((D(D) * (lists:nth((0)+1, Factors10))) + D).

pad(N) ->
    S2 = lists:flatten(io_lib:format("~p", [N])),
    (fun Loop4(S) -> case (length(S) < 4) of true -> S3 = " " ++ S, Loop4(S3); _ -> ok end end(S2)),
    S3.

main() ->
    Vals0 = [],
    N0 = -99,
    (fun Loop5(N, Vals) -> case (N < 101) of true -> Vals1 = Vals ++ [(D(N))], N1 = (N + 1), Loop5(Vals1, N1); _ -> ok end end(N0, Vals0)),
    I2 = 0,
    (fun Loop7(I) -> case (I < length(Vals1)) of true -> Line0 = "", J0 = 0, (fun Loop6(Line, J) -> case (J < 10) of true -> Line1 = (Line + pad(lists:nth(((I + J))+1, Vals1))), (case (J < 9) of true -> Line2 = Line ++ " "; _ -> ok end), J1 = (J + 1), Loop6(Line2, J1); _ -> ok end end(Line0, J0)), io:format("~p~n", [Line2]), I3 = (I + 10), Loop7(I3); _ -> ok end end(I2)),
    Pow0 = 1,
    M0 = 1,
    (fun Loop8(M, Pow) -> case (M < 21) of true -> Pow1 = (Pow * 10), Exp0 = lists:flatten(io_lib:format("~p", [M])), (case (length(Exp0) < 2) of true -> Exp1 = Exp0 ++ " "; _ -> ok end), Res0 = (lists:flatten(io_lib:format("~p", [M])) + repeat("0", (M - 1))), io:format("~p~n", ["D(10^" ++ Exp1 ++ ") / 7 = " ++ Res0]), M1 = (M + 1), Loop8(Pow1, M1); _ -> ok end end(M0, Pow0)).

main(_) ->
    main().
