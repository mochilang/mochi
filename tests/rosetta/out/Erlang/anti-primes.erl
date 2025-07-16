% anti-primes.erl - generated from anti-primes.mochi

countDivisors(N) ->
    (case (N < 2) of true -> 1; _ -> ok end),
    Count0 = 2,
    I0 = 2,
    (fun Loop0(I) -> case (I =< (N / 2)) of true -> (case (rem(N, I) == 0) of true -> Count1 = (Count0 + 1); _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    Count1.

main() ->
    io:format("~p~n", ["The first 20 anti-primes are:"]),
    MaxDiv0 = 0,
    Count2 = 0,
    N0 = 1,
    Line0 = "",
    (fun Loop1(N) -> case (Count2 < 20) of true -> D = countDivisors(N), (case (D > MaxDiv0) of true -> Line1 = (Line0 + lists:flatten(io_lib:format("~p", [N]))) ++ " ", MaxDiv1 = D, Count3 = (Count2 + 1); _ -> ok end), N1 = (N + 1), Loop1(N1); _ -> ok end end(N0)),
    Line2 = string:substr(Line1, (0)+1, ((length(Line1) - 1))-(0)),
    io:format("~p~n", [Line2]).

main(_) ->
    main().
