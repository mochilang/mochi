% atomic-updates.erl - generated from atomic-updates.mochi

randOrder(Seed, N) ->
    Next = rem((((Seed * 1664525) + 1013904223)), 2147483647),
    [Next, rem(Next, N)].

randChaos(Seed, N) ->
    Next = rem((((Seed * 1103515245) + 12345)), 2147483647),
    [Next, rem(Next, N)].

main() ->
    Buckets0 = [],
    {Buckets1} = lists:foldl(fun(I, {Buckets}) -> Buckets1 = Buckets ++ [0], {Buckets1} end, {Buckets0}, lists:seq(0, (NBuckets)-1)),
    I0 = NBuckets,
    Dist0 = InitialSum,
    (fun Loop0(I, Buckets, Dist) -> case (I > 0) of true -> V = (Dist / I), I1 = (I - 1), Buckets2 = lists:sublist(Buckets, I) ++ [V] ++ lists:nthtail((I)+1, Buckets), Dist1 = (Dist - V), Loop0(I1, Buckets2, Dist1); _ -> ok end end(I0, Buckets1, Dist0)),
    Tc00 = 0,
    Tc10 = 0,
    Total0 = 0,
    NTicks0 = 0,
    SeedOrder0 = 1,
    SeedChaos0 = 2,
    io:format("~p~n", ["sum  ---updates---    mean  buckets"]),
    T0 = 0,
    (fun Loop2(SeedOrder, R, B1, NTicks, Tc0, SeedChaos, B2, Buckets, Tc1, Total, T) -> case (T < 5) of true -> R0 = randOrder(SeedOrder, NBuckets), SeedOrder1 = lists:nth((0)+1, R), B10 = lists:nth((1)+1, R), B20 = rem(((B1 + 1)), NBuckets), V1 = lists:nth((B1)+1, Buckets), V2 = lists:nth((B2)+1, Buckets), (case (V1 > V2) of true -> A0 = ((((V1 - V2)) / 2)), (case (A0 > lists:nth((B1)+1, Buckets)) of true -> A1 = lists:nth((B1)+1, Buckets); _ -> ok end), Buckets3 = lists:sublist(Buckets, B1) ++ [(lists:nth((B1)+1, Buckets) - A1)] ++ lists:nthtail((B1)+1, Buckets), Buckets4 = lists:sublist(Buckets, B2) ++ [(lists:nth((B2)+1, Buckets) + A1)] ++ lists:nthtail((B2)+1, Buckets); _ -> A2 = ((((V2 - V1)) / 2)), (case (A2 > lists:nth((B2)+1, Buckets)) of true -> A3 = lists:nth((B2)+1, Buckets); _ -> ok end), Buckets5 = lists:sublist(Buckets, B2) ++ [(lists:nth((B2)+1, Buckets) - A3)] ++ lists:nthtail((B2)+1, Buckets), Buckets6 = lists:sublist(Buckets, B1) ++ [(lists:nth((B1)+1, Buckets) + A3)] ++ lists:nthtail((B1)+1, Buckets) end), Tc01 = (Tc0 + 1), R1 = randChaos(SeedChaos, NBuckets), SeedChaos1 = lists:nth((0)+1, R), B11 = lists:nth((1)+1, R), B21 = rem(((B1 + 1)), NBuckets), R2 = randChaos(SeedChaos, (lists:nth((B1)+1, Buckets) + 1)), SeedChaos2 = lists:nth((0)+1, R), Amt0 = lists:nth((1)+1, R), (case (Amt0 > lists:nth((B1)+1, Buckets)) of true -> Amt1 = lists:nth((B1)+1, Buckets); _ -> ok end), Buckets7 = lists:sublist(Buckets, B1) ++ [(lists:nth((B1)+1, Buckets) - Amt1)] ++ lists:nthtail((B1)+1, Buckets), Buckets8 = lists:sublist(Buckets, B2) ++ [(lists:nth((B2)+1, Buckets) + Amt1)] ++ lists:nthtail((B2)+1, Buckets), Tc11 = (Tc1 + 1), Sum0 = 0, Idx0 = 0, (fun Loop1(Sum, Idx) -> case (Idx < NBuckets) of true -> Sum1 = (Sum + lists:nth((Idx)+1, Buckets)), Idx1 = (Idx + 1), Loop1(Sum1, Idx1); _ -> ok end end(Sum0, Idx0)), Total1 = ((Total + Tc0) + Tc1), NTicks1 = (NTicks + 1), io:format("~p~n", [lists:flatten(io_lib:format("~p", [Sum1])) ++ " " ++ lists:flatten(io_lib:format("~p", [Tc0])) ++ " " ++ lists:flatten(io_lib:format("~p", [Tc1])) ++ " " ++ lists:flatten(io_lib:format("~p", [(Total / NTicks)])) ++ "  " ++ lists:flatten(io_lib:format("~p", [Buckets]))]), Tc02 = 0, Tc12 = 0, T1 = (T + 1), Loop2(NTicks1, Tc02, SeedChaos2, B21, Buckets8, Tc12, Total1, T1, SeedOrder1, R2, B11); _ -> ok end end(SeedOrder0, R, B1, NTicks0, Tc00, SeedChaos0, B2, Buckets2, Tc10, Total0, T0)).

main(_) ->
    main().
