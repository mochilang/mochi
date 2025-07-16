% arithmetic-geometric-mean.erl - generated from arithmetic-geometric-mean.mochi

abs(X) ->
    case (X < 0) of true -> -X; _ -> X end.

sqrtApprox(X) ->
    Guess0 = X,
    I0 = 0,
    (fun Loop0(Guess, I) -> case (I < 20) of true -> Guess1 = (((Guess + (X / Guess))) / 2), I1 = (I + 1), Loop0(Guess1, I1); _ -> ok end end(Guess0, I0)),
    Guess1.

agm(A, G) ->
    (fun Loop1(A, G) -> case (abs((A - G)) > (abs(A) * Eps)) of true -> NewA = (((A + G)) / 2), NewG = sqrtApprox((A * G)), A0 = NewA, G0 = NewG, Loop1(A0, G0); _ -> ok end end(A, G)),
    A0.

main() ->
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [agm(1, (1 / sqrtApprox(2)))]))]).

main(_) ->
    main().
