% attractive-numbers.erl - generated from attractive-numbers.mochi

isPrime(N) ->
    (case (N < 2) of true -> false; _ -> ok end),
    (case (rem(N, 2) == 0) of true -> (N == 2); _ -> ok end),
    (case (rem(N, 3) == 0) of true -> (N == 3); _ -> ok end),
    D0 = 5,
    (fun Loop0(D) -> case ((D * D) =< N) of true -> (case (rem(N, D) == 0) of true -> false; _ -> ok end), D1 = (D + 2), (case (rem(N, D) == 0) of true -> false; _ -> ok end), D2 = (D + 4), Loop0(D2); _ -> ok end end(D0)),
    true.

countPrimeFactors(N) ->
    (case (N == 1) of true -> 0; _ -> ok end),
    (case isPrime(N) of undefined -> ok; false -> ok; _ -> 1 end),
    Count0 = 0,
    F0 = 2,
    (fun Loop1() -> case true of true -> (case (rem(N, F0) == 0) of true -> Count1 = (Count0 + 1), N0 = (N / F0), (case (N0 == 1) of true -> Count1; _ -> ok end), (case isPrime(N0) of undefined -> ok; false -> ok; _ -> F1 = N0 end); _ -> (case (F1 >= 3) of true -> F2 = (F1 + 2); _ -> F3 = 3 end) end), Loop1(); _ -> ok end end()),
    Count1.

pad4(N) ->
    S0 = lists:flatten(io_lib:format("~p", [N0])),
    (fun Loop2(S) -> case (length(S) < 4) of true -> S1 = " " ++ S, Loop2(S1); _ -> ok end end(S0)),
    S1.

main() ->
    io:format("~p~n", ["The attractive numbers up to and including " ++ lists:flatten(io_lib:format("~p", [Max])) ++ " are:"]),
    Count2 = 0,
    Line0 = "",
    LineCount0 = 0,
    I0 = 1,
    (fun Loop3(I) -> case (I =< Max) of true -> C = countPrimeFactors(I), (case isPrime(C) of undefined -> ok; false -> ok; _ -> Line1 = (Line0 + pad4(I)), Count3 = (Count2 + 1), LineCount1 = (LineCount0 + 1), (case (LineCount1 == 20) of true -> io:format("~p~n", [Line1]), Line2 = "", LineCount2 = 0; _ -> ok end) end), I1 = (I + 1), Loop3(I1); _ -> ok end end(I0)),
    (case (LineCount2 > 0) of true -> io:format("~p~n", [Line2]); _ -> ok end).

main(_) ->
    main().
