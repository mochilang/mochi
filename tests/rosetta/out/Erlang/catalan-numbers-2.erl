% catalan-numbers-2.erl - generated from catalan-numbers-2.mochi

catalanRec(N) ->
    (case (N == 0) of true -> 1; _ -> ok end),
    T10 = (2 * N),
    T20 = (T10 - 1),
    T30 = (2 * T20),
    T50 = (T30 * catalanRec((N - 1))),
    ((T50 / ((N + 1)))).

main() ->
    lists:foreach(fun(I) -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [catalanRec(I)]))]) end, lists:seq(1, (16)-1)).

main(_) ->
    main().
