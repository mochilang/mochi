% active-object.erl - generated from active-object.mochi

sinApprox(X) ->
    Term0 = X,
    Sum0 = X,
    N0 = 1,
    (fun Loop0(Term, Sum, N) -> case (N =< 12) of true -> Denom = ((((2 * N)) * (((2 * N) + 1)))), Term1 = (((-Term * X) * X) / Denom), Sum1 = (Sum + Term), N1 = (N + 1), Loop0(Term1, Sum1, N1); _ -> ok end end(Term0, Sum0, N0)),
    Sum1.

main(_) ->
    S0 = 0,
    T10 = 0,
    K10 = sinApprox(0),
    I0 = 1,
    (fun Loop1(I, S, T1, K1) -> case (I =< 200) of true -> T2 = ((I) * 0.01), K2 = sinApprox((T2 * 3.141592653589793)), S1 = (S + ((((K1 + K2)) * 0.5) * ((T2 - T1)))), T11 = T2, K11 = K2, I1 = (I + 1), Loop1(T11, K11, I1, S1); _ -> ok end end(I0, S0, T10, K10)),
    I20 = 1,
    (fun Loop2(S, T1, K1, I2) -> case (I2 =< 50) of true -> T2 = (2 + ((I2) * 0.01)), , S2 = (S + ((((K1 + K2)) * 0.5) * ((T2 - T1)))), T12 = T2, K12 = K2, I21 = (I2 + 1), Loop2(S2, T12, K12, I21); _ -> ok end end(S1, T11, K11, I20)),
    io:format("~p~n", [S2]).
