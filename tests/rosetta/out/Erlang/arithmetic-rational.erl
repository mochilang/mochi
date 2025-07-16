% arithmetic-rational.erl - generated from arithmetic-rational.mochi

intSqrt(X) ->
    (case (X < 2) of true -> X; _ -> ok end),
    Left0 = 1,
    Right0 = (X / 2),
    Ans0 = 0,
    (fun Loop0() -> case (Left0 =< Right0) of true -> Mid = (Left0 + (((Right0 - Left0)) / 2)), Sq = (Mid * Mid), (case (Sq == X) of true -> Mid; _ -> ok end), (case (Sq < X) of true -> Left1 = (Mid + 1), Ans1 = Mid; _ -> Right1 = (Mid - 1) end), Loop0(); _ -> ok end end()),
    Ans1.

sumRecip(N) ->
    S0 = 1,
    Limit = intSqrt(N),
    F0 = 2,
    (fun Loop1(F) -> case (F =< Limit) of true -> (case (rem(N, F) == 0) of true -> S1 = (S0 + (N / F)), F2 = (N / F), (case (F2 /= F) of true -> S2 = (S1 + F); _ -> ok end); _ -> ok end), F1 = (F + 1), Loop1(F1); _ -> ok end end(F0)),
    S2.

main() ->
    Nums = [6, 28, 120, 496, 672, 8128, 30240, 32760, 523776],
    lists:foreach(fun(N) -> S = sumRecip(N), (case (rem(S, N) == 0) of true -> Val = (S / N), Perfect0 = "", (case (Val == 1) of true -> Perfect1 = "perfect!"; _ -> ok end), io:format("~p~n", ["Sum of recipr. factors of " ++ lists:flatten(io_lib:format("~p", [N])) ++ " = " ++ lists:flatten(io_lib:format("~p", [Val])) ++ " exactly " ++ Perfect1]); _ -> ok end) end, Nums).

main(_) ->
    main().
