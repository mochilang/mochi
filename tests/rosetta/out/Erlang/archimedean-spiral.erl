% archimedean-spiral.erl - generated from archimedean-spiral.mochi

sinApprox(X) ->
    Term0 = X,
    Sum0 = X,
    N0 = 1,
    (fun Loop0(Term, Sum, N) -> case (N =< 10) of true -> Denom = ((((2 * N)) * (((2 * N) + 1)))), Term1 = (((-Term * X) * X) / Denom), Sum1 = (Sum + Term), N1 = (N + 1), Loop0(Term1, Sum1, N1); _ -> ok end end(Term0, Sum0, N0)),
    Sum1.

cosApprox(X) ->
    Term2 = 1,
    Sum2 = 1,
    N2 = 1,
    (fun Loop1(Term, Sum, N) -> case (N =< 10) of true -> Denom = (((((2 * N) - 1)) * ((2 * N)))), Term3 = (((-Term * X) * X) / Denom), Sum3 = (Sum + Term), N3 = (N + 1), Loop1(Sum3, N3, Term3); _ -> ok end end(Term2, Sum2, N2)),
    Sum3.

main(_) ->
    DegreesIncr = ((0.1 * 3.141592653589793) / 180),
    Stop = (((360 * 2) * 10) * DegreesIncr),
    Centre = (600 / 2),
    Theta0 = 0,
    Count0 = 0,
    (fun Loop2(Theta, Count) -> case (Theta < Stop) of true -> R = (1 + (20 * Theta)), X = (R * cosApprox(Theta)), Y = (R * sinApprox(Theta)), (case (rem(Count, 100) == 0) of true -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [(Centre + X)])) ++ "," ++ lists:flatten(io_lib:format("~p", [(Centre - Y)]))]); _ -> ok end), Theta1 = (Theta + DegreesIncr), Count1 = (Count + 1), Loop2(Theta1, Count1); _ -> ok end end(Theta0, Count0)).
