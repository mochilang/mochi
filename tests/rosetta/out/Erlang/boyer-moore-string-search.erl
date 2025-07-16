% boyer-moore-string-search.erl - generated from boyer-moore-string-search.mochi

indexOfStr(H, N) ->
    Hlen = length(H),
    Nlen = length(N),
    (case (Nlen == 0) of true -> 0; _ -> ok end),
    I0 = 0,
    (fun Loop0(I) -> case (I =< (Hlen - Nlen)) of true -> (case (string:substr(H, (I)+1, ((I + Nlen))-(I)) == N) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    -1.

stringSearchSingle(H, N) ->
    indexOfStr(H, N).

stringSearch(H, N) ->
    Result0 = [],
    Start0 = 0,
    Hlen = length(H),
    Nlen = length(N),
    (fun Loop1() -> case (Start0 < Hlen) of true -> Idx = indexOfStr(string:substr(H, (Start0)+1, (Hlen)-(Start0)), N), (case (Idx >= 0) of true -> Result1 = Result0 ++ [(Start0 + Idx)], Start1 = ((Start0 + Idx) + Nlen); _ -> throw(break) end), Loop1(); _ -> ok end end()),
    Result1.

display(Nums) ->
    S0 = "[",
    I2 = 0,
    (fun Loop2(S, I) -> case (I < length(Nums)) of true -> (case (I > 0) of true -> S1 = S ++ ", "; _ -> ok end), S2 = (S + lists:flatten(io_lib:format("~p", [lists:nth((I)+1, Nums)]))), I3 = (I + 1), Loop2(S2, I3); _ -> ok end end(S0, I2)),
    S3 = S2 ++ "]",
    S3.

main() ->
    Texts = ["GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."],
    Patterns = ["TCTA", "TAATAAA", "word", "needle", "and", "alfalfa"],
    I4 = 0,
    (fun Loop3(I) -> case (I < length(Texts)) of true -> io:format("~p~n", ["text" ++ lists:flatten(io_lib:format("~p", [(I + 1)])) ++ " = " ++ lists:nth((I)+1, Texts)]), I5 = (I + 1), Loop3(I5); _ -> ok end end(I4)),
    io:format("~p~n", [""]),
    J0 = 0,
    (fun Loop4(J) -> case (J < length(Texts)) of true -> Idxs = stringSearch(lists:nth((J)+1, Texts), lists:nth((J)+1, Patterns)), io:format("~p~n", ["Found \"" ++ lists:nth((J)+1, Patterns) ++ "\" in 'text" ++ lists:flatten(io_lib:format("~p", [(J + 1)])) ++ "' at indexes " ++ display(Idxs)]), J1 = (J + 1), Loop4(J1); _ -> ok end end(J0)).

main(_) ->
    main().
