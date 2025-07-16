% burrows-wheeler-transform.erl - generated from burrows-wheeler-transform.mochi

contains(S, Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> true; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    false.

sortStrings(Xs) ->
    Arr0 = Xs,
    N0 = length(Arr0),
    I2 = 0,
    (fun Loop2(I) -> case (I < N0) of true -> J0 = 0, (fun Loop1(J) -> case (J < (N0 - 1)) of true -> (case (mochi_get(J, Arr0) > mochi_get((J + 1), Arr0)) of true -> Tmp = mochi_get(J, Arr0), Arr1 = maps:put(J, mochi_get((J + 1), Arr0), Arr0), Arr2 = maps:put((J + 1), Tmp, Arr1); _ -> ok end), J1 = (J + 1), Loop1(J1); _ -> ok end end(J0)), I3 = (I + 1), Loop2(I3); _ -> ok end end(I2)),
    Arr2.

bwt(S) ->
    (case (contains(S, "\x02") orelse contains(S, "\x03")) of true -> #{"err" => true, "res" => ""}; _ -> ok end),
    S0 = (("\x02" + S) + "\x03"),
    Le = length(S0),
    Table0 = [],
    I4 = 0,
    (fun Loop3(Table, I) -> case (I < Le) of true -> Rot = (string:substr(S0, (I)+1, (Le)-(I)) + string:substr(S0, (0)+1, (I)-(0))), Table1 = Table ++ [Rot], I5 = (I + 1), Loop3(I5, Table1); _ -> ok end end(Table0, I4)),
    Table2 = sortStrings(Table1),
    Last0 = "",
    I6 = 0,
    (fun Loop4(Last, I) -> case (I < Le) of true -> Last1 = (Last + string:substr(lists:nth((I)+1, Table2), ((Le - 1))+1, (Le)-((Le - 1)))), I7 = (I + 1), Loop4(I7, Last1); _ -> ok end end(Last0, I6)),
    #{"err" => false, "res" => Last1}.

ibwt(R) ->
    Le = length(R),
    Table3 = [],
    I8 = 0,
    (fun Loop5(Table, I) -> case (I < Le) of true -> Table4 = Table ++ [""], I9 = (I + 1), Loop5(Table4, I9); _ -> ok end end(Table3, I8)),
    N1 = 0,
    (fun Loop7(N, I, Table) -> case (N < Le) of true -> I10 = 0, (fun Loop6(Table, I) -> case (I < Le) of true -> Table5 = lists:sublist(Table, I) ++ [(string:substr(R, (I)+1, ((I + 1))-(I)) + lists:nth((I)+1, Table))] ++ lists:nthtail((I)+1, Table), I11 = (I + 1), Loop6(Table5, I11); _ -> ok end end(Table, I)), Table6 = sortStrings(Table5), N2 = (N + 1), Loop7(N2, I11, Table6); _ -> ok end end(N1, I9, Table4)),
    I12 = 0,
    (fun Loop8(I) -> case (I < Le) of true -> (case (string:substr(lists:nth((I)+1, Table6), ((Le - 1))+1, (Le)-((Le - 1))) == "\x03") of true -> string:substr(lists:nth((I)+1, Table6), (1)+1, ((Le - 1))-(1)); _ -> ok end), I13 = (I + 1), Loop8(I13); _ -> ok end end(I12)),
    "".

makePrintable(S) ->
    Out0 = "",
    I14 = 0,
    (fun Loop9(I) -> case (I < length(S0)) of true -> Ch = string:substr(S0, (I)+1, ((I + 1))-(I)), (case (Ch == "\x02") of true -> Out1 = Out0 ++ "^"; _ -> (case (Ch == "\x03") of true -> Out2 = Out1 ++ "|"; _ -> Out3 = (Out2 + Ch) end) end), I15 = (I + 1), Loop9(I15); _ -> ok end end(I14)),
    Out3.

main() ->
    Examples = ["banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES", "\x02ABC\x03"],
    lists:foreach(fun(T) -> io:format("~p~n", [makePrintable(T)]), Res = bwt(T), (case mochi_get("err", Res) of undefined -> Enc = mochi_get("res", Res), io:format("~p~n", [" --> " ++ makePrintable(Enc)]), R = ibwt(Enc), io:format("~p~n", [" --> " ++ R]); false -> Enc = mochi_get("res", Res), io:format("~p~n", [" --> " ++ makePrintable(Enc)]), R = ibwt(Enc), io:format("~p~n", [" --> " ++ R]); _ -> io:format("~p~n", [" --> ERROR: String can't contain STX or ETX"]), io:format("~p~n", [" -->"]) end), io:format("~p~n", [""]) end, Examples).

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
