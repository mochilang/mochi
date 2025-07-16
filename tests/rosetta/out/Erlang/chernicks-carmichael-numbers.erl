% chernicks-carmichael-numbers.erl - generated from chernicks-carmichael-numbers.mochi

isPrime(N) ->
    (case (N < 2) of true -> false; _ -> ok end),
    (case (rem(N, 2) == 0) of true -> (N == 2); _ -> ok end),
    (case (rem(N, 3) == 0) of true -> (N == 3); _ -> ok end),
    D0 = 5,
    (fun Loop0(D) -> case ((D * D) =< N) of true -> (case (rem(N, D) == 0) of true -> false; _ -> ok end), D1 = (D + 2), (case (rem(N, D) == 0) of true -> false; _ -> ok end), D2 = (D + 4), Loop0(D2); _ -> ok end end(D0)),
    true.

bigTrim(A) ->
    N0 = (case A of #{items := It} -> length(It); _ -> length(A) end),
    (fun Loop1(A, N) -> case ((N > 1) andalso (lists:nth(((N - 1))+1, A) == 0)) of true -> A0 = lists:sublist(A, (0)+1, ((N - 1))-(0)), N1 = (N - 1), Loop1(A0, N1); _ -> ok end end(A, N0)),
    A0.

bigFromInt(X) ->
    (case (X == 0) of true -> [0]; _ -> ok end),
    Digits0 = [],
    N2 = X,
    (fun Loop2(Digits, N) -> case (N > 0) of true -> Digits1 = Digits ++ [rem(N, 10)], N3 = (N / 10), Loop2(Digits1, N3); _ -> ok end end(Digits0, N2)),
    Digits1.

bigMulSmall(A, M) ->
    (case (M == 0) of true -> [0]; _ -> ok end),
    Res0 = [],
    Carry0 = 0,
    I0 = 0,
    (fun Loop3(Res, Carry, I) -> case (I < (case A0 of #{items := It} -> length(It); _ -> length(A0) end)) of true -> Prod0 = ((lists:nth((I)+1, A0) * M) + Carry), Res1 = Res ++ [rem(Prod0, 10)], Carry1 = (Prod0 / 10), I1 = (I + 1), Loop3(Res1, Carry1, I1); _ -> ok end end(Res0, Carry0, I0)),
    (fun Loop4(Res, Carry) -> case (Carry > 0) of true -> Res2 = Res ++ [rem(Carry, 10)], Carry2 = (Carry / 10), Loop4(Res2, Carry2); _ -> ok end end(Res1, Carry1)),
    bigTrim(Res2).

bigToString(A) ->
    S0 = "",
    I2 = ((case A0 of #{items := It} -> length(It); _ -> length(A0) end) - 1),
    (fun Loop5(S, I) -> case (I >= 0) of true -> S1 = (S + lists:flatten(io_lib:format("~p", [lists:nth((I)+1, A0)]))), I3 = (I - 1), Loop5(S1, I3); _ -> ok end end(S0, I2)),
    S1.

pow2(K) ->
    R0 = 1,
    I4 = 0,
    (fun Loop6(I, R) -> case (I < K) of true -> R1 = (R * 2), I5 = (I + 1), Loop6(R1, I5); _ -> ok end end(I4, R0)),
    R1.

ccFactors(N, M) ->
    P0 = ((6 * M) + 1),
    (case not isPrime(P0) of true -> []; _ -> ok end),
    Prod1 = bigFromInt(P0),
    P1 = ((12 * M) + 1),
    (case not isPrime(P1) of true -> []; _ -> ok end),
    Prod2 = bigMulSmall(Prod1, P1),
    I6 = 1,
    (fun Loop7(P, Prod, I) -> case (I =< (N3 - 2)) of true -> P2 = ((((pow2(I) * 9) * M)) + 1), (case not isPrime(P) of true -> []; _ -> ok end), Prod3 = bigMulSmall(Prod, P), I7 = (I + 1), Loop7(P2, Prod3, I7); _ -> ok end end(P1, Prod2, I6)),
    Prod3.

ccNumbers(Start, End) ->
    N4 = Start,
    (fun Loop9(N) -> case (N =< End) of true -> M0 = 1, (case (N > 4) of true -> M1 = pow2((N - 4)); _ -> ok end), (fun Loop8() -> case true of true -> Num = ccFactors(N, M1), (case ((case Num of #{items := It} -> length(It); _ -> length(Num) end) > 0) of true -> io:format("~p~n", ["a(" ++ lists:flatten(io_lib:format("~p", [N])) ++ ") = " ++ bigToString(Num)]), throw(break); _ -> ok end), (case (N =< 4) of true -> M2 = (M1 + 1); _ -> M3 = (M2 + pow2((N - 4))) end), Loop8(); _ -> ok end end()), N5 = (N + 1), Loop9(N5); _ -> ok end end(N4)).

main(_) ->
    ccNumbers(3, 9).
