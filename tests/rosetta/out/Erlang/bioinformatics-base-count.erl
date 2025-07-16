% bioinformatics-base-count.erl - generated from bioinformatics-base-count.mochi

padLeft(S, W) ->
    Res0 = "",
    N0 = (W - length(S)),
    (fun Loop0(Res, N) -> case (N > 0) of true -> Res1 = Res ++ " ", N1 = (N - 1), Loop0(Res1, N1); _ -> ok end end(Res0, N0)),
    (Res1 + S).

main(_) ->
    Dna = "" ++ "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG" ++ "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG" ++ "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT" ++ "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT" ++ "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG" ++ "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA" ++ "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT" ++ "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG" ++ "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC" ++ "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT",
    io:format("~p~n", ["SEQUENCE:"]),
    Le = length(Dna),
    I0 = 0,
    (fun Loop1(I) -> case (I < Le) of true -> K0 = (I + 50), (case (K0 > Le) of true -> K1 = Le; _ -> ok end), io:format("~p~n", [padLeft(lists:flatten(io_lib:format("~p", [I])), 5) ++ ": " ++ lists:sublist(Dna, (I)+1, (K1)-(I))]), I1 = (I + 50), Loop1(I1); _ -> ok end end(I0)),
    A0 = 0,
    C0 = 0,
    G0 = 0,
    T0 = 0,
    Idx0 = 0,
    (fun Loop2(Idx) -> case (Idx < Le) of true -> Ch = string:substr(Dna, (Idx)+1, ((Idx + 1))-(Idx)), (case (Ch == "A") of true -> A1 = (A0 + 1); _ -> (case (Ch == "C") of true -> C1 = (C0 + 1); _ -> (case (Ch == "G") of true -> G1 = (G0 + 1); _ -> (case (Ch == "T") of true -> T1 = (T0 + 1); _ -> ok end) end) end) end), Idx1 = (Idx + 1), Loop2(Idx1); _ -> ok end end(Idx0)),
    io:format("~p~n", [""]),
    io:format("~p~n", ["BASE COUNT:"]),
    io:format("~p~n", ["    A: " ++ padLeft(lists:flatten(io_lib:format("~p", [A1])), 3)]),
    io:format("~p~n", ["    C: " ++ padLeft(lists:flatten(io_lib:format("~p", [C1])), 3)]),
    io:format("~p~n", ["    G: " ++ padLeft(lists:flatten(io_lib:format("~p", [G1])), 3)]),
    io:format("~p~n", ["    T: " ++ padLeft(lists:flatten(io_lib:format("~p", [T1])), 3)]),
    io:format("~p~n", ["    ------"]),
    io:format("~p~n", ["    Σ: " ++ lists:flatten(io_lib:format("~p", [Le]))]),
    io:format("~p~n", ["    ======"]).
