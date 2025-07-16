% circular-primes.erl - generated from circular-primes.mochi

isPrime(N) ->
    (case (N < 2) of true -> false; _ -> ok end),
    (case (rem(N, 2) == 0) of true -> (N == 2); _ -> ok end),
    (case (rem(N, 3) == 0) of true -> (N == 3); _ -> ok end),
    D0 = 5,
    (fun Loop0(D) -> case ((D * D) =< N) of true -> (case (rem(N, D) == 0) of true -> false; _ -> ok end), D1 = (D + 2), (case (rem(N, D) == 0) of true -> false; _ -> ok end), D2 = (D + 4), Loop0(D2); _ -> ok end end(D0)),
    true.

isCircular(N) ->
    Nn0 = N,
    Pow0 = 1,
    (fun Loop1(Pow, Nn) -> case (Nn > 0) of true -> Pow1 = (Pow * 10), Nn1 = (Nn / 10), Loop1(Pow1, Nn1); _ -> ok end end(Pow0, Nn0)),
    Nn2 = N,
    (fun Loop2(Nn) -> case true of true -> Nn3 = (Nn * 10), F = (Nn / Pow1), Nn4 = (Nn + (F * ((1 - Pow1)))), (case (Nn == N) of true -> throw(break); _ -> ok end), (case not isPrime(Nn) of true -> false; _ -> ok end), Loop2(Nn4); _ -> ok end end(Nn2)),
    true.

showList(Xs) ->
    Out0 = "[",
    I0 = 0,
    (fun Loop3(Out, I) -> case (I < length(Xs)) of true -> Out1 = (Out + lists:flatten(io_lib:format("~p", [lists:nth((I)+1, Xs)]))), (case (I < (length(Xs) - 1)) of true -> Out2 = Out ++ ", "; _ -> ok end), I1 = (I + 1), Loop3(Out2, I1); _ -> ok end end(Out0, I0)),
    Out2 ++ "]".

main(_) ->
    Circs0 = [],
    io:format("~p~n", ["The first 19 circular primes are:"]),
    Digits0 = [1, 3, 7, 9],
    Q0 = [1, 2, 3, 5, 7, 9],
    Fq0 = [1, 2, 3, 5, 7, 9],
    Count0 = 0,
    (fun Loop4(Q, Fq) -> case true of true -> F = lists:nth((0)+1, Q), Fd = lists:nth((0)+1, Fq), (case (isPrime(F) andalso isCircular(F)) of true -> Circs1 = Circs0 ++ [F], Count1 = (Count0 + 1), (case (Count1 == 19) of true -> throw(break); _ -> ok end); _ -> ok end), Q1 = lists:sublist(Q, (1)+1, (length(Q))-(1)), Fq1 = lists:sublist(Fq, (1)+1, (length(Fq))-(1)), (case ((F /= 2) andalso (F /= 5)) of true -> {Q2, Fq2} = lists:foldl(fun(D, {Q, Fq}) -> Q2 = Q ++ [((F * 10) + D2)], Fq2 = Fq ++ [Fd], {Q2, Fq2} end, {Q, Fq}, Digits0); _ -> ok end), Loop4(Q2, Fq2); _ -> ok end end(Q0, Fq0)),
    io:format("~p~n", [showList(Circs1)]),
    io:format("~p~n", ["\nThe next 4 circular primes, in repunit format, are:"]),
    io:format("~p~n", ["[R(19) R(23) R(317) R(1031)]"]),
    io:format("~p~n", ["\nThe following repunits are probably circular primes:"]),
    lists:foreach(fun(I) -> io:format("~p~n", ["R(" ++ lists:flatten(io_lib:format("~p", [I1])) ++ ") : true"]) end, [5003, 9887, 15073, 25031, 35317, 49081]).
