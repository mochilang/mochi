% catalan-numbers-1.erl - generated from catalan-numbers-1.mochi

binom(N, K) ->
    (case ((K < 0) orelse (K > N)) of true -> 0; _ -> ok end),
    Kk0 = K,
    (case (Kk0 > (N - Kk0)) of true -> Kk1 = (N - Kk0); _ -> ok end),
    Res0 = 1,
    I0 = 0,
    (fun Loop0(Res, I) -> case (I < Kk1) of true -> Res1 = ((Res * ((N - I)))), I1 = (I + 1), Res2 = ((Res / I)), Loop0(Res2, I1); _ -> ok end end(Res0, I0)),
    Res2.

catalan(N) ->
    ((binom((2 * N), N) / ((N + 1)))).

main() ->
    lists:foreach(fun(I) -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [catalan(I1)]))]) end, lists:seq(0, (15)-1)).

main(_) ->
    main().
