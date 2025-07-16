% bioinformatics-sequence-mutation.erl - generated from bioinformatics-sequence-mutation.mochi

randInt(S, N) ->
    Next = rem((((S * 1664525) + 1013904223)), 2147483647),
    [Next, rem(Next, N)].

padLeft(S, W) ->
    Res0 = "",
    N0 = (W - length(S)),
    (fun Loop0(Res, N) -> case (N > 0) of true -> Res1 = Res ++ " ", N1 = (N - 1), Loop0(Res1, N1); _ -> ok end end(Res0, N0)),
    (Res1 + S).

makeSeq(S, Le) ->
    Out0 = "",
    I0 = 0,
    (fun Loop1(I, S, Out) -> case (I < Le) of true -> R0 = randInt(S, 4), S0 = lists:nth((0)+1, R0), Idx = lists:nth((1)+1, R0), Out1 = (Out + string:substr(Bases, (Idx)+1, ((Idx + 1))-(Idx))), I1 = (I + 1), Loop1(S0, Out1, I1); _ -> ok end end(I0, S, Out0)),
    [S0, Out1].

mutate(S, Dna, W) ->
    Le = length(Dna),
    R1 = randInt(S0, Le),
    S1 = lists:nth((0)+1, R1),
    P = lists:nth((1)+1, R1),
    R2 = randInt(S1, 300),
    S2 = lists:nth((0)+1, R2),
    X = lists:nth((1)+1, R2),
    Arr0 = [],
    I2 = 0,
    (fun Loop2(I, Arr) -> case (I < Le) of true -> Arr1 = Arr ++ [string:substr(Dna, (I)+1, ((I + 1))-(I))], I3 = (I + 1), Loop2(Arr1, I3); _ -> ok end end(I2, Arr0)),
    (case (X < lists:nth((0)+1, W)) of true -> R3 = randInt(S2, 4), S3 = lists:nth((0)+1, R3), Idx = lists:nth((1)+1, R3), B = string:substr(Bases, (Idx)+1, ((Idx + 1))-(Idx)), io:format("~p~n", ["  Change @" ++ padLeft(lists:flatten(io_lib:format("~p", [P])), 3) ++ " '" ++ lists:nth((P)+1, Arr1) ++ "' to '" ++ B ++ "'"]), Arr2 = lists:sublist(Arr1, P) ++ [B] ++ lists:nthtail((P)+1, Arr1); _ -> (case (X < (lists:nth((0)+1, W) + lists:nth((1)+1, W))) of true -> io:format("~p~n", ["  Delete @" ++ padLeft(lists:flatten(io_lib:format("~p", [P])), 3) ++ " '" ++ lists:nth((P)+1, Arr2) ++ "'"]), J0 = P, (fun Loop3(Arr, J) -> case (J < (length(Arr) - 1)) of true -> Arr3 = lists:sublist(Arr, J) ++ [lists:nth(((J + 1))+1, Arr)] ++ lists:nthtail((J)+1, Arr), J1 = (J + 1), Loop3(Arr3, J1); _ -> ok end end(Arr2, J0)), Arr4 = lists:sublist(Arr3, (0)+1, ((length(Arr3) - 1))-(0)); _ -> R4 = randInt(S3, 4), S4 = lists:nth((0)+1, R4), Idx2 = lists:nth((1)+1, R4), B = string:substr(Bases, (Idx2)+1, ((Idx2 + 1))-(Idx2)), Arr5 = Arr4 ++ [""], J2 = (length(Arr5) - 1), (fun Loop4(Arr, J) -> case (J > P) of true -> Arr6 = lists:sublist(Arr, J) ++ [lists:nth(((J - 1))+1, Arr)] ++ lists:nthtail((J)+1, Arr), J3 = (J - 1), Loop4(J3, Arr6); _ -> ok end end(Arr5, J2)), io:format("~p~n", ["  Insert @" ++ padLeft(lists:flatten(io_lib:format("~p", [P])), 3) ++ " '" ++ B ++ "'"]), Arr7 = lists:sublist(Arr6, P) ++ [B] ++ lists:nthtail((P)+1, Arr6) end) end),
    Out2 = "",
    I4 = 0,
    (fun Loop5(Out, I) -> case (I < length(Arr7)) of true -> Out3 = (Out + lists:nth((I)+1, Arr7)), I5 = (I + 1), Loop5(Out3, I5); _ -> ok end end(Out2, I4)),
    [S4, Out3].

prettyPrint(Dna, RowLen) ->
    io:format("~p~n", ["SEQUENCE:"]),
    Le = length(Dna),
    I6 = 0,
    (fun Loop6(I) -> case (I < Le) of true -> K0 = (I + RowLen), (case (K0 > Le) of true -> K1 = Le; _ -> ok end), io:format("~p~n", [padLeft(lists:flatten(io_lib:format("~p", [I])), 5) ++ ": " ++ string:substr(Dna, (I)+1, (K1)-(I))]), I7 = (I + RowLen), Loop6(I7); _ -> ok end end(I6)),
    A0 = 0,
    C0 = 0,
    G0 = 0,
    T0 = 0,
    Idx0 = 0,
    (fun Loop7(Idx) -> case (Idx < Le) of true -> Ch = string:substr(Dna, (Idx)+1, ((Idx + 1))-(Idx)), (case (Ch == "A") of true -> A1 = (A0 + 1); _ -> (case (Ch == "C") of true -> C1 = (C0 + 1); _ -> (case (Ch == "G") of true -> G1 = (G0 + 1); _ -> (case (Ch == "T") of true -> T1 = (T0 + 1); _ -> ok end) end) end) end), Idx1 = (Idx + 1), Loop7(Idx1); _ -> ok end end(Idx)),
    io:format("~p~n", [""]),
    io:format("~p~n", ["BASE COUNT:"]),
    io:format("~p~n", ["    A: " ++ padLeft(lists:flatten(io_lib:format("~p", [A1])), 3)]),
    io:format("~p~n", ["    C: " ++ padLeft(lists:flatten(io_lib:format("~p", [C1])), 3)]),
    io:format("~p~n", ["    G: " ++ padLeft(lists:flatten(io_lib:format("~p", [G1])), 3)]),
    io:format("~p~n", ["    T: " ++ padLeft(lists:flatten(io_lib:format("~p", [T1])), 3)]),
    io:format("~p~n", ["    ------"]),
    io:format("~p~n", ["    Σ: " ++ lists:flatten(io_lib:format("~p", [Le]))]),
    io:format("~p~n", ["    ======"]).

wstring(W) ->
    "  Change: " ++ lists:flatten(io_lib:format("~p", [lists:nth((0)+1, W)])) ++ "\n  Delete: " ++ lists:flatten(io_lib:format("~p", [lists:nth((1)+1, W)])) ++ "\n  Insert: " ++ lists:flatten(io_lib:format("~p", [lists:nth((2)+1, W)])) ++ "\n".

main() ->
    Seed0 = 1,
    Res2 = makeSeq(Seed0, 250),
    Seed1 = lists:nth((0)+1, Res2),
    Dna0 = lists:nth((1)+1, Res2),
    prettyPrint(Dna0, 50),
    W = [100, 100, 100],
    io:format("~p~n", ["\nWEIGHTS (ex 300):"]),
    io:format("~p~n", [wstring(W)]),
    io:format("~p~n", ["MUTATIONS (" ++ lists:flatten(io_lib:format("~p", [Muts])) ++ "):"]),
    I8 = 0,
    (fun Loop8(Dna, I, Res, Seed) -> case (I < Muts) of true -> Res3 = mutate(Seed, Dna, W), Seed2 = lists:nth((0)+1, Res), Dna1 = lists:nth((1)+1, Res), I9 = (I + 1), Loop8(Res3, Seed2, Dna1, I9); _ -> ok end end(Dna0, I8, Res2, Seed1)),
    io:format("~p~n", [""]),
    prettyPrint(Dna1, 50).

main(_) ->
    main().
