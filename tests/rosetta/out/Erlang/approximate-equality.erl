% approximate-equality.erl - generated from approximate-equality.mochi

abs(X) ->
    case (X < 0) of true -> -X; _ -> X end.

maxf(A, B) ->
    case (A > B) of true -> A; _ -> B end.

isClose(A, B) ->
    T = abs((A - B)),
    U = (RelTol * maxf(abs(A), abs(B))),
    (T =< U).

sqrtApprox(X) ->
    Guess0 = X,
    I0 = 0,
    (fun Loop0(Guess, I) -> case (I < 10) of true -> Guess1 = (((Guess + (X / Guess))) / 2), I1 = (I + 1), Loop0(Guess1, I1); _ -> ok end end(Guess0, I0)),
    Guess1.

main() ->
    Root2 = sqrtApprox(2),
    Pairs = [[1.0000000000000002e+14, 1.0000000000000002e+14], [100.01, 100.011], [(1.0000000000000002e+13 / 10000), 1.0000000000000001e+09], [0.001, 0.0010000001], [1.01e-22, 0], [(Root2 * Root2), 2], [((-Root2) * Root2), -2], [1e+17, 1e+17], [3.141592653589793, 3.141592653589793]],
    lists:foreach(fun(Pair) -> A = lists:nth((0)+1, Pair), B = lists:nth((1)+1, Pair), S = (case isClose(A, B) of undefined -> "≉"; false -> "≉"; _ -> "≈" end), io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " " ++ S ++ " " ++ lists:flatten(io_lib:format("~p", [B]))]) end, Pairs).

main(_) ->
    main().
