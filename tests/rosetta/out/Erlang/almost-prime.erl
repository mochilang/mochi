% almost-prime.erl - generated from almost-prime.mochi

kPrime(N, K) ->
    Nf0 = 0,
    I0 = 2,
    (fun Loop1(I) -> case (I =< N) of true -> (fun Loop0(Nf, N) -> case (rem(N, I) == 0) of true -> (case (Nf == K) of true -> false; _ -> ok end), Nf1 = (Nf + 1), N0 = (N / I), Loop0(Nf1, N0); _ -> ok end end(Nf0, N)), I1 = (I + 1), Loop1(I1); _ -> ok end end(I0)),
    (Nf1 == K).

gen(K, Count) ->
    R0 = [],
    N1 = 2,
    (fun Loop2(N) -> case (length(R0) < Count) of true -> (case kPrime(N, K) of undefined -> ok; false -> ok; _ -> R1 = R0 ++ [N] end), N2 = (N + 1), Loop2(N2); _ -> ok end end(N1)),
    R1.

main() ->
    K0 = 1,
    (fun Loop3(K) -> case (K =< 5) of true -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [K])) ++ " " ++ lists:flatten(io_lib:format("~p", [gen(K, 10)]))]), K1 = (K + 1), Loop3(K1); _ -> ok end end(K0)).

main(_) ->
    main().
