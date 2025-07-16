% arithmetic-numbers.erl - generated from arithmetic-numbers.mochi

sieve(Limit) ->
    Spf0 = [],
    I0 = 0,
    (fun Loop0(Spf, I) -> case (I =< Limit) of true -> Spf1 = Spf ++ [0], I1 = (I + 1), Loop0(Spf1, I1); _ -> ok end end(Spf0, I0)),
    I2 = 2,
    (fun Loop2(I) -> case (I =< Limit) of true -> (case (lists:nth((I)+1, Spf1) == 0) of true -> Spf2 = lists:sublist(Spf1, I) ++ [I] ++ lists:nthtail((I)+1, Spf1), (case ((I * I) =< Limit) of true -> J0 = (I * I), (fun Loop1(J) -> case (J =< Limit) of true -> (case (lists:nth((J)+1, Spf2) == 0) of true -> Spf3 = lists:sublist(Spf2, J) ++ [I] ++ lists:nthtail((J)+1, Spf2); _ -> ok end), J1 = (J + I), Loop1(J1); _ -> ok end end(J0)); _ -> ok end); _ -> ok end), I3 = (I + 1), Loop2(I3); _ -> ok end end(I2)),
    Spf3.

primesFrom(Spf, Limit) ->
    Primes0 = [],
    I4 = 3,
    (fun Loop3(I) -> case (I =< Limit) of true -> (case (lists:nth((I)+1, Spf3) == I) of true -> Primes1 = Primes0 ++ [I]; _ -> ok end), I5 = (I + 1), Loop3(I5); _ -> ok end end(I4)),
    Primes1.

pad3(N) ->
    S0 = lists:flatten(io_lib:format("~p", [N])),
    (fun Loop4(S) -> case (length(S) < 3) of true -> S1 = " " ++ S, Loop4(S1); _ -> ok end end(S0)),
    S1.

commatize(N) ->
    S2 = lists:flatten(io_lib:format("~p", [N])),
    Out0 = "",
    I6 = (length(S2) - 1),
    C0 = 0,
    (fun Loop5(Out, C, I) -> case (I >= 0) of true -> Out1 = (string:substr(S2, (I)+1, ((I + 1))-(I)) + Out), C1 = (C + 1), (case ((rem(C, 3) == 0) andalso (I > 0)) of true -> Out2 = "," ++ Out; _ -> ok end), I7 = (I - 1), Loop5(Out2, C1, I7); _ -> ok end end(Out0, C0, I6)),
    Out2.

primeCount(Primes, Last, Spf) ->
    Lo0 = 0,
    Hi0 = length(Primes1),
    (fun Loop6() -> case (Lo0 < Hi0) of true -> Mid0 = ((((Lo0 + Hi0)) / 2)), (case (lists:nth((Mid0)+1, Primes1) < Last) of true -> Lo1 = (Mid0 + 1); _ -> Hi1 = Mid0 end), Loop6(); _ -> ok end end()),
    Count0 = (Lo1 + 1),
    (case (lists:nth((Last)+1, Spf3) /= Last) of true -> Count1 = (Count0 - 1); _ -> ok end),
    Count1.

arithmeticNumbers(Limit, Spf) ->
    Arr0 = [1],
    N0 = 3,
    (fun Loop9(N) -> case (length(Arr0) < Limit) of true -> (case (lists:nth((N)+1, Spf3) == N) of true -> Arr1 = Arr0 ++ [N]; _ -> X0 = N, Sigma0 = 1, Tau0 = 1, (fun Loop8(Sigma, Tau) -> case (X0 > 1) of true -> P0 = lists:nth((X0)+1, Spf3), (case (P0 == 0) of true -> P1 = X0; _ -> ok end), Cnt0 = 0, Power0 = P1, Sum0 = 1, (fun Loop7(X, Cnt, Sum, Power) -> case (rem(X, P1) == 0) of true -> X1 = (X / P1), Cnt1 = (Cnt + 1), Sum1 = (Sum + Power), Power1 = (Power * P1), Loop7(Sum1, Power1, X1, Cnt1); _ -> ok end end(X0, Cnt0, Sum0, Power0)), Sigma1 = (Sigma * Sum1), Tau1 = (Tau * ((Cnt1 + 1))), Loop8(Sigma1, Tau1); _ -> ok end end(Sigma0, Tau0)), (case (rem(Sigma1, Tau1) == 0) of true -> Arr2 = Arr1 ++ [N]; _ -> ok end) end), N1 = (N + 1), Loop9(N1); _ -> ok end end(N0)),
    Arr2.

main() ->
    Spf = sieve(Limit),
    Primes = primesFrom(Spf, Limit),
    Arr = arithmeticNumbers(1000000, Spf),
    io:format("~p~n", ["The first 100 arithmetic numbers are:"]),
    I8 = 0,
    (fun Loop11(I) -> case (I < 100) of true -> Line0 = "", J2 = 0, (fun Loop10(Line, J) -> case (J < 10) of true -> Line1 = (Line + pad3(lists:nth(((I + J))+1, Arr))), (case (J < 9) of true -> Line2 = Line ++ " "; _ -> ok end), J3 = (J + 1), Loop10(Line2, J3); _ -> ok end end(Line0, J2)), io:format("~p~n", [Line2]), I9 = (I + 10), Loop11(I9); _ -> ok end end(I8)),
    lists:foreach(fun(X) -> Last = lists:nth(((X1 - 1))+1, Arr), Lastc = commatize(Last), io:format("~p~n", ["\nThe " ++ commatize(X1) ++ "th arithmetic number is: " ++ Lastc]), Pc = primeCount(Primes, Last, Spf), Comp = ((X1 - Pc) - 1), io:format("~p~n", ["The count of such numbers <= " ++ Lastc ++ " which are composite is " ++ commatize(Comp) ++ "."]) end, [1000, 10000, 100000, 1000000]).

main(_) ->
    main().
