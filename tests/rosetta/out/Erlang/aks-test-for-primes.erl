% aks-test-for-primes.erl - generated from aks-test-for-primes.mochi

poly(P) ->
    S0 = "",
    Coef0 = 1,
    I0 = P,
    (case (Coef0 /= 1) of true -> S1 = (S0 + lists:flatten(io_lib:format("~p", [Coef0]))); _ -> ok end),
    (fun Loop0(Coef, I, S) -> case (I > 0) of true -> S2 = S ++ "x", (case (I /= 1) of true -> S3 = S ++ "^" ++ lists:flatten(io_lib:format("~p", [I])); _ -> ok end), Coef1 = (((Coef * I) / (((P - I) + 1)))), D0 = Coef, (case (rem(((P - ((I - 1)))), 2) == 1) of true -> D1 = -D0; _ -> ok end), (case (D1 < 0) of true -> S4 = S ++ " - " ++ lists:flatten(io_lib:format("~p", [-D1])); _ -> S5 = S ++ " + " ++ lists:flatten(io_lib:format("~p", [D1])) end), I1 = (I - 1), Loop0(S5, Coef1, I1); _ -> ok end end(Coef0, I0, S1)),
    (case (S5 == "") of true -> S6 = "1"; _ -> ok end),
    S6.

aks(N) ->
    (case (N < 2) of true -> false; _ -> ok end),
    C0 = N,
    I2 = 1,
    (fun Loop1(I, C) -> case (I < N) of true -> (case (rem(C, N) /= 0) of true -> false; _ -> ok end), C1 = (((C * ((N - I))) / ((I + 1)))), I3 = (I + 1), Loop1(C1, I3); _ -> ok end end(I2, C0)),
    true.

main() ->
    P0 = 0,
    (fun Loop2(P) -> case (P =< 7) of true -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [P])) ++ ":  " ++ poly(P)]), P1 = (P + 1), Loop2(P1); _ -> ok end end(P0)),
    First0 = true,
    P2 = 2,
    Line0 = "",
    (fun Loop3(P) -> case (P < 50) of true -> (case aks(P) of undefined -> ok; false -> ok; _ -> (case First0 of undefined -> Line2 = Line1 ++ " " ++ lists:flatten(io_lib:format("~p", [P])); false -> Line2 = Line1 ++ " " ++ lists:flatten(io_lib:format("~p", [P])); _ -> Line1 = (Line0 + lists:flatten(io_lib:format("~p", [P]))), First1 = false end) end), P3 = (P + 1), Loop3(P3); _ -> ok end end(P2)),
    io:format("~p~n", [Line2]).

main(_) ->
    main().
