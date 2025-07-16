% best-shuffle.erl - generated from best-shuffle.mochi

nextRand(Seed) ->
    rem((((Seed * 1664525) + 1013904223)), 2147483647).

shuffleChars(S, Seed) ->
    Chars0 = [],
    I0 = 0,
    (fun Loop0(Chars, I) -> case (I < length(S)) of true -> Chars1 = Chars ++ [string:substr(S, (I)+1, ((I + 1))-(I))], I1 = (I + 1), Loop0(Chars1, I1); _ -> ok end end(Chars0, I0)),
    Sd0 = Seed,
    Idx0 = (length(Chars1) - 1),
    (fun Loop1(Sd, Chars, Idx) -> case (Idx > 0) of true -> Sd1 = nextRand(Sd), J0 = rem(Sd, ((Idx + 1))), Tmp = lists:nth((Idx)+1, Chars), Chars2 = lists:sublist(Chars, Idx) ++ [lists:nth((J0)+1, Chars)] ++ lists:nthtail((Idx)+1, Chars), Chars3 = lists:sublist(Chars, J0) ++ [Tmp] ++ lists:nthtail((J0)+1, Chars), Idx1 = (Idx - 1), Loop1(Sd1, Chars3, Idx1); _ -> ok end end(Sd0, Chars1, Idx0)),
    Res0 = "",
    I2 = 0,
    (fun Loop2(Res, I) -> case (I < length(Chars3)) of true -> Res1 = (Res + lists:nth((I)+1, Chars3)), I3 = (I + 1), Loop2(Res1, I3); _ -> ok end end(Res0, I2)),
    [Res1, Sd1].

bestShuffle(S, Seed) ->
    R = shuffleChars(S, Seed),
    T0 = lists:nth((0)+1, R),
    Sd2 = lists:nth((1)+1, R),
    Arr0 = [],
    I4 = 0,
    (fun Loop3(Arr, I) -> case (I < length(T0)) of true -> Arr1 = Arr ++ [string:substr(T0, (I)+1, ((I + 1))-(I))], I5 = (I + 1), Loop3(Arr1, I5); _ -> ok end end(Arr0, I4)),
    I6 = 0,
    (fun Loop5(I) -> case (I < length(Arr1)) of true -> J1 = 0, (fun Loop4(J) -> case (J < length(Arr1)) of true -> (case (((I /= J) andalso (lists:nth((I)+1, Arr1) /= string:substr(S, (J)+1, ((J + 1))-(J)))) andalso (lists:nth((J)+1, Arr1) /= string:substr(S, (I)+1, ((I + 1))-(I)))) of true -> Tmp = lists:nth((I)+1, Arr1), Arr2 = lists:sublist(Arr1, I) ++ [lists:nth((J)+1, Arr1)] ++ lists:nthtail((I)+1, Arr1), Arr3 = lists:sublist(Arr2, J) ++ [Tmp] ++ lists:nthtail((J)+1, Arr2), throw(break); _ -> ok end), J2 = (J + 1), Loop4(J2); _ -> ok end end(J1)), I7 = (I + 1), Loop5(I7); _ -> ok end end(I6)),
    Count0 = 0,
    I8 = 0,
    (fun Loop6(I) -> case (I < length(Arr3)) of true -> (case (lists:nth((I)+1, Arr3) == string:substr(S, (I)+1, ((I + 1))-(I))) of true -> Count1 = (Count0 + 1); _ -> ok end), I9 = (I + 1), Loop6(I9); _ -> ok end end(I8)),
    Out0 = "",
    I10 = 0,
    (fun Loop7(Out, I) -> case (I < length(Arr3)) of true -> Out1 = (Out + lists:nth((I)+1, Arr3)), I11 = (I + 1), Loop7(Out1, I11); _ -> ok end end(Out0, I10)),
    [Out1, Sd2, Count1].

main() ->
    Ts = ["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"],
    Seed0 = 1,
    I12 = 0,
    (fun Loop8(Seed, I) -> case (I < length(Ts)) of true -> R = bestShuffle(lists:nth((I)+1, Ts), Seed), Shuf = lists:nth((0)+1, R), Seed1 = lists:nth((1)+1, R), Cnt = lists:nth((2)+1, R), io:format("~p~n", [lists:nth((I)+1, Ts) ++ " -> " ++ Shuf ++ " (" ++ lists:flatten(io_lib:format("~p", [Cnt])) ++ ")"]), I13 = (I + 1), Loop8(Seed1, I13); _ -> ok end end(Seed0, I12)).

main(_) ->
    main().
