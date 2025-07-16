% arithmetic-geometric-mean-calculate-pi.erl - generated from arithmetic-geometric-mean-calculate-pi.mochi

abs(X) ->
    case (X < 0) of true -> -X; _ -> X end.

sqrtApprox(X) ->
    Guess0 = X,
    I0 = 0,
    (fun Loop0(Guess, I) -> case (I < 20) of true -> Guess1 = (((Guess + (X / Guess))) / 2), I1 = (I + 1), Loop0(I1, Guess1); _ -> ok end end(Guess0, I0)),
    Guess1.

agmPi() ->
    A0 = 1,
    G0 = (1 / sqrtApprox(2)),
    Sum0 = 0,
    Pow0 = 2,
    (fun Loop1(A, G, Pow, Sum) -> case (abs((A - G)) > 1e-15) of true -> T0 = (((A + G)) / 2), U0 = sqrtApprox((A * G)), A1 = T0, G1 = U0, Pow1 = (Pow * 2), Diff0 = ((A * A) - (G * G)), Sum1 = (Sum + (Diff0 * Pow)), Loop1(A1, G1, Pow1, Sum1); _ -> ok end end(A0, G0, Pow0, Sum0)),
    Pi0 = (((4 * A1) * A1) / ((1 - Sum1))),
    Pi0.

main() ->
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [agmPi()]))]).

main(_) ->
    main().
