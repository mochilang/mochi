% averages-root-mean-square.erl - generated from averages-root-mean-square.mochi

sqrtApprox(X) ->
    Guess0 = X,
    I0 = 0,
    (fun Loop0(Guess, I) -> case (I < 20) of true -> Guess1 = (((Guess + (X / Guess))) / 2), I1 = (I + 1), Loop0(Guess1, I1); _ -> ok end end(Guess0, I0)),
    Guess1.

main(_) ->
    Sum0 = 0,
    X0 = 1,
    (fun Loop1(Sum, X) -> case (X =< 10) of true -> Sum1 = (Sum + ((X) * (X))), X1 = (X + 1), Loop1(Sum1, X1); _ -> ok end end(Sum0, X0)),
    Rms = sqrtApprox((Sum1 / (10))),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Rms]))]).
