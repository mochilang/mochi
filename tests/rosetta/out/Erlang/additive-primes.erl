% additive-primes.erl - generated from additive-primes.mochi

isPrime(N) ->
    (case (N < 2) of true -> false; _ -> ok end),
    (case (rem(N, 2) == 0) of true -> (N == 2); _ -> ok end),
    (case (rem(N, 3) == 0) of true -> (N == 3); _ -> ok end),
    D0 = 5,
    (fun Loop0(D) -> case ((D * D) =< N) of true -> (case (rem(N, D) == 0) of true -> false; _ -> ok end), D1 = (D + 2), (case (rem(N, D) == 0) of true -> false; _ -> ok end), D2 = (D + 4), Loop0(D2); _ -> ok end end(D0)),
    true.

sumDigits(N) ->
    S0 = 0,
    X0 = N,
    (fun Loop1(S, X) -> case (X > 0) of true -> S1 = (S + rem(X, 10)), X1 = ((X / 10)), Loop1(X1, S1); _ -> ok end end(S0, X0)),
    S1.

pad(N) ->
    (case (N < 10) of true -> "  " ++ lists:flatten(io_lib:format("~p", [N])); _ -> ok end),
    (case (N < 100) of true -> " " ++ lists:flatten(io_lib:format("~p", [N])); _ -> ok end),
    lists:flatten(io_lib:format("~p", [N])).

main() ->
    io:format("~p~n", ["Additive primes less than 500:"]),
    Count0 = 0,
    Line0 = "",
    LineCount0 = 0,
    I0 = 2,
    (fun Loop2() -> case (I0 < 500) of true -> (case (isPrime(I0) andalso isPrime(sumDigits(I0))) of true -> Count1 = (Count0 + 1), Line1 = (Line0 + pad(I0)) ++ "  ", LineCount1 = (LineCount0 + 1), (case (LineCount1 == 10) of true -> io:format("~p~n", [string:substr(Line1, (0)+1, ((length(Line1) - 2))-(0))]), Line2 = "", LineCount2 = 0; _ -> ok end); _ -> ok end), (case (I0 > 2) of true -> I1 = (I0 + 2); _ -> I2 = (I1 + 1) end), Loop2(); _ -> ok end end()),
    (case (LineCount2 > 0) of true -> io:format("~p~n", [string:substr(Line2, (0)+1, ((length(Line2) - 2))-(0))]); _ -> ok end),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Count1])) ++ " additive primes found."]).

main(_) ->
    main().
