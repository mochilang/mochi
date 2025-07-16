% amicable-pairs.erl - generated from amicable-pairs.mochi

pfacSum(I) ->
    Sum0 = 0,
    P0 = 1,
    (fun Loop0(P) -> case (P =< (I / 2)) of true -> (case (rem(I, P) == 0) of true -> Sum1 = (Sum0 + P); _ -> ok end), P1 = (P + 1), Loop0(P1); _ -> ok end end(P0)),
    Sum1.

pad(N, Width) ->
    S0 = lists:flatten(io_lib:format("~p", [N])),
    (fun Loop1(S) -> case (length(S) < Width) of true -> S1 = " " ++ S, Loop1(S1); _ -> ok end end(S0)),
    S1.

main() ->
    Sums0 = [],
    I0 = 0,
    (fun Loop2(Sums, I) -> case (I < 20000) of true -> Sums1 = Sums ++ [0], I1 = (I + 1), Loop2(Sums1, I1); _ -> ok end end(Sums0, I0)),
    I2 = 1,
    (fun Loop3(I, Sums) -> case (I < 20000) of true -> Sums2 = lists:sublist(Sums, I) ++ [pfacSum(I)] ++ lists:nthtail((I)+1, Sums), I3 = (I + 1), Loop3(Sums2, I3); _ -> ok end end(I2, Sums1)),
    io:format("~p~n", ["The amicable pairs below 20,000 are:"]),
    N0 = 2,
    (fun Loop4(N) -> case (N < 19999) of true -> M = lists:nth((N)+1, Sums2), (case (((M > N) andalso (M < 20000)) andalso (N == lists:nth((M)+1, Sums2))) of true -> io:format("~p~n", ["  " ++ pad(N, 5) ++ " and " ++ pad(M, 5)]); _ -> ok end), N1 = (N + 1), Loop4(N1); _ -> ok end end(N0)).

main(_) ->
    main().
