% averages-pythagorean-means.erl - generated from averages-pythagorean-means.mochi

powf(Base, Exp) ->
    Result0 = 1,
    I0 = 0,
    (fun Loop0(Result, I) -> case (I < Exp) of true -> Result1 = (Result * Base), I1 = (I + 1), Loop0(Result1, I1); _ -> ok end end(Result0, I0)),
    Result1.

nthRoot(X, N) ->
    Low0 = 0,
    High0 = X,
    I2 = 0,
    (fun Loop1(I) -> case (I < 60) of true -> Mid = (((Low0 + High0)) / 2), (case (powf(Mid, N) > X) of true -> High1 = Mid; _ -> Low1 = Mid end), I3 = (I + 1), Loop1(I3); _ -> ok end end(I2)),
    Low1.

main() ->
    Sum0 = 0,
    SumRecip0 = 0,
    Prod0 = 1,
    N0 = 1,
    (fun Loop2(Sum, SumRecip, Prod, N) -> case (N =< 10) of true -> F = N, Sum1 = (Sum + F), SumRecip1 = (SumRecip + (1 / F)), Prod1 = (Prod * F), N1 = (N + 1), Loop2(Sum1, SumRecip1, Prod1, N1); _ -> ok end end(Sum0, SumRecip0, Prod0, N0)),
    A = (Sum1 / Count),
    G = nthRoot(Prod1, 10),
    H = (Count / SumRecip1),
    io:format("~p~n", ["A: " ++ lists:flatten(io_lib:format("~p", [A])) ++ " G: " ++ lists:flatten(io_lib:format("~p", [G])) ++ " H: " ++ lists:flatten(io_lib:format("~p", [H]))]),
    io:format("~p~n", ["A >= G >= H: " ++ lists:flatten(io_lib:format("~p", [((A >= G) andalso (G >= H))]))]).

main(_) ->
    main().
