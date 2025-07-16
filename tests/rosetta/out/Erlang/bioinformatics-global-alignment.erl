% bioinformatics-global-alignment.erl - generated from bioinformatics-global-alignment.mochi

padLeft(S, W) ->
    Res0 = "",
    N0 = (W - length(S)),
    (fun Loop0(Res, N) -> case (N > 0) of true -> Res1 = Res ++ " ", N1 = (N - 1), Loop0(Res1, N1); _ -> ok end end(Res0, N0)),
    (Res1 + S).

indexOfFrom(S, Ch, Start) ->
    I0 = Start,
    (fun Loop1(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop1(I1); _ -> ok end end(I0)),
    -1.

containsStr(S, Sub) ->
    I2 = 0,
    Sl = length(S),
    Subl = length(Sub),
    (fun Loop2(I) -> case (I =< (Sl - Subl)) of true -> (case (string:substr(S, (I)+1, ((I + Subl))-(I)) == Sub) of true -> true; _ -> ok end), I3 = (I + 1), Loop2(I3); _ -> ok end end(I2)),
    false.

distinct(Slist) ->
    Res2 = [],
    try lists:foreach(fun(S) -> try Found0 = false, try lists:foreach(fun(R) -> try (case (R == S) of true -> Found1 = true, throw(break); _ -> ok end) catch throw:continue -> ok end end, Res2) catch throw:break -> ok end, (case not Found1 of true -> Res3 = Res2 ++ [S]; _ -> ok end) catch throw:continue -> ok end end, Slist) catch throw:break -> ok end,
    Res3.

permutations(Xs) ->
    (case (length(Xs) =< 1) of true -> [Xs]; _ -> ok end),
    Res4 = [],
    I4 = 0,
    (fun Loop5(I) -> case (I < length(Xs)) of true -> Rest0 = [], J0 = 0, (fun Loop3(J) -> case (J < length(Xs)) of true -> (case (J /= I) of true -> Rest1 = Rest0 ++ [lists:nth((J)+1, Xs)]; _ -> ok end), J1 = (J + 1), Loop3(J1); _ -> ok end end(J0)), Subs = permutations(Rest1), {Res5} = lists:foldl(fun(P, {Res}) -> Perm0 = [lists:nth((I)+1, Xs)], K0 = 0, (fun Loop4(Perm, K) -> case (K < length(P)) of true -> Perm1 = Perm ++ [mochi_get(K, P)], K1 = (K + 1), Loop4(Perm1, K1); _ -> ok end end(Perm0, K0)), Res5 = Res ++ [Perm1], {Res5} end, {Res4}, Subs), I5 = (I + 1), Loop5(I5); _ -> ok end end(I4)),
    Res5.

headTailOverlap(S1, S2) ->
    Start0 = 0,
    (fun Loop6(Start) -> case true of true -> Ix = indexOfFrom(S1, string:substr(S2, (0)+1, (1)-(0)), Start), (case (Ix == (0 - 1)) of true -> 0; _ -> ok end), Start1 = Ix, (case (string:substr(S2, (0)+1, ((length(S1) - Start))-(0)) == string:substr(S1, (Start)+1, (length(S1))-(Start))) of true -> (length(S1) - Start); _ -> ok end), Start2 = (Start + 1), Loop6(Start2); _ -> ok end end(Start0)).

deduplicate(Slist) ->
    Arr = distinct(Slist),
    Filtered0 = [],
    I6 = 0,
    (fun Loop8(I) -> case (I < length(Arr)) of true -> S1 = mochi_get(I, Arr), Within0 = false, J2 = 0, (fun Loop7(J) -> case (J < length(Arr)) of true -> (case ((J /= I) andalso containsStr(mochi_get(J, Arr), S1)) of true -> Within1 = true, throw(break); _ -> ok end), J3 = (J + 1), Loop7(J3); _ -> ok end end(J2)), (case not Within1 of true -> Filtered1 = Filtered0 ++ [S1]; _ -> ok end), I7 = (I + 1), Loop8(I7); _ -> ok end end(I6)),
    Filtered1.

joinAll(Ss) ->
    Out0 = "",
    {Out1} = lists:foldl(fun(S, {Out}) -> Out1 = (Out + S), {Out1} end, {Out0}, Ss),
    Out1.

shortestCommonSuperstring(Slist) ->
    Ss = deduplicate(Slist),
    Shortest0 = joinAll(Ss),
    Perms = permutations(Ss),
    Idx0 = 0,
    (fun Loop10(Idx) -> case (Idx < length(Perms)) of true -> Perm = mochi_get(Idx, Perms), Sup0 = lists:nth((0)+1, Perm), I8 = 0, (fun Loop9(Sup, I) -> case (I < (length(Ss) - 1)) of true -> Ov = headTailOverlap(lists:nth((I)+1, Perm), lists:nth(((I + 1))+1, Perm)), Sup1 = (Sup + string:substr(lists:nth(((I + 1))+1, Perm), (Ov)+1, (length(lists:nth(((I + 1))+1, Perm)))-(Ov))), I9 = (I + 1), Loop9(Sup1, I9); _ -> ok end end(Sup0, I8)), (case (length(Sup1) < length(Shortest0)) of true -> Shortest1 = Sup1; _ -> ok end), Idx1 = (Idx + 1), Loop10(Idx1); _ -> ok end end(Idx0)),
    Shortest1.

printCounts(Seq) ->
    A0 = 0,
    C0 = 0,
    G0 = 0,
    T0 = 0,
    I10 = 0,
    (fun Loop11(I) -> case (I < length(Seq)) of true -> Ch = string:substr(Seq, (I)+1, ((I + 1))-(I)), (case (Ch == "A") of true -> A1 = (A0 + 1); _ -> (case (Ch == "C") of true -> C1 = (C0 + 1); _ -> (case (Ch == "G") of true -> G1 = (G0 + 1); _ -> (case (Ch == "T") of true -> T1 = (T0 + 1); _ -> ok end) end) end) end), I11 = (I + 1), Loop11(I11); _ -> ok end end(I10)),
    Total = length(Seq),
    io:format("~p~n", ["\nNucleotide counts for " ++ Seq ++ ":\n"]),
    io:format("~p~n", [(padLeft("A", 10) + padLeft(lists:flatten(io_lib:format("~p", [A1])), 12))]),
    io:format("~p~n", [(padLeft("C", 10) + padLeft(lists:flatten(io_lib:format("~p", [C1])), 12))]),
    io:format("~p~n", [(padLeft("G", 10) + padLeft(lists:flatten(io_lib:format("~p", [G1])), 12))]),
    io:format("~p~n", [(padLeft("T", 10) + padLeft(lists:flatten(io_lib:format("~p", [T1])), 12))]),
    io:format("~p~n", [(padLeft("Other", 10) + padLeft(lists:flatten(io_lib:format("~p", [(Total - ((((A1 + C1) + G1) + T1)))])), 12))]),
    io:format("~p~n", ["  ____________________"]),
    io:format("~p~n", [(padLeft("Total length", 14) + padLeft(lists:flatten(io_lib:format("~p", [Total])), 8))]).

main() ->
    Tests = [["TA", "AAG", "TA", "GAA", "TA"], ["CATTAGGG", "ATTAG", "GGG", "TA"], ["AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"], ["ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"]],
    lists:foreach(fun(Seqs) -> Scs = shortestCommonSuperstring(Seqs), printCounts(Scs) end, Tests).

main(_) ->
    main().

mochi_get(K, M) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error ->
            Name = atom_to_list(K),
            case string:tokens(Name, "_") of
                [Pref|_] ->
                    P = list_to_atom(Pref),
                    case maps:find(P, M) of
                        {ok, Sub} when is_map(Sub) -> maps:get(K, Sub, undefined);
                        _ -> undefined
                    end;
                _ -> undefined
            end
        end.
