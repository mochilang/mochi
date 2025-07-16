% canonicalize-cidr.erl - generated from canonicalize-cidr.mochi

split(S, Sep) ->
    Parts0 = [],
    Cur0 = "",
    I0 = 0,
    (fun Loop0() -> case (I0 < length(S)) of true -> (case (((length(Sep) > 0) andalso ((I0 + length(Sep)) =< length(S))) andalso (string:substr(S, (I0)+1, ((I0 + length(Sep)))-(I0)) == Sep)) of true -> Parts1 = Parts0 ++ [Cur0], Cur1 = "", I1 = (I0 + length(Sep)); _ -> Cur2 = (Cur1 + string:substr(S, (I1)+1, ((I1 + 1))-(I1))), I2 = (I1 + 1) end), Loop0(); _ -> ok end end()),
    Parts2 = Parts1 ++ [Cur2],
    Parts2.

join(Xs, Sep) ->
    Res0 = "",
    I3 = 0,
    (fun Loop1(Res, I) -> case (I < length(Xs)) of true -> (case (I > 0) of true -> Res1 = (Res + Sep); _ -> ok end), Res2 = (Res + lists:nth((I)+1, Xs)), I4 = (I + 1), Loop1(Res2, I4); _ -> ok end end(Res0, I3)),
    Res2.

repeat(Ch, N) ->
    Out0 = "",
    I5 = 0,
    (fun Loop2(Out, I) -> case (I < N) of true -> Out1 = (Out + Ch), I6 = (I + 1), Loop2(Out1, I6); _ -> ok end end(Out0, I5)),
    Out1.

parseIntStr(Str) ->
    I7 = 0,
    Neg0 = false,
    (case ((length(Str) > 0) andalso (string:substr(Str, (0)+1, (1)-(0)) == "-")) of true -> Neg1 = true, I8 = 1; _ -> ok end),
    N0 = 0,
    Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
    (fun Loop3(N, I) -> case (I < length(Str)) of true -> N1 = ((N * 10) + mochi_get(string:substr(Str, (I)+1, ((I + 1))-(I)), Digits)), I9 = (I + 1), Loop3(N1, I9); _ -> ok end end(N0, I8)),
    (case Neg1 of undefined -> ok; false -> ok; _ -> N2 = -N1 end),
    N2.

toBinary(N, Bits) ->
    B0 = "",
    Val0 = N2,
    I10 = 0,
    (fun Loop4(B, Val, I) -> case (I < Bits) of true -> B1 = (lists:flatten(io_lib:format("~p", [rem(Val, 2)])) + B), Val1 = ((Val / 2)), I11 = (I + 1), Loop4(B1, Val1, I11); _ -> ok end end(B0, Val0, I10)),
    B1.

binToInt(Bits) ->
    N3 = 0,
    I12 = 0,
    (fun Loop5(N, I) -> case (I < length(Bits)) of true -> N4 = ((N * 2) + parseIntStr(string:substr(Bits, (I)+1, ((I + 1))-(I)))), I13 = (I + 1), Loop5(N4, I13); _ -> ok end end(N3, I12)),
    N4.

padRight(S, Width) ->
    Out2 = S,
    (fun Loop6(Out) -> case (length(Out) < Width) of true -> Out3 = Out ++ " ", Loop6(Out3); _ -> ok end end(Out2)),
    Out3.

canonicalize(Cidr) ->
    Parts = split(Cidr, "/"),
    Dotted = lists:nth((0)+1, Parts),
    Size = parseIntStr(lists:nth((1)+1, Parts)),
    BinParts0 = [],
    {BinParts1} = lists:foldl(fun(P, {BinParts}) -> BinParts1 = BinParts ++ [toBinary(parseIntStr(P), 8)], {BinParts1} end, {BinParts0}, split(Dotted, ".")),
    Binary0 = join(BinParts1, ""),
    Binary1 = (lists:sublist(Binary0, (0)+1, (Size)-(0)) + repeat("0", (32 - Size))),
    CanonParts0 = [],
    I14 = 0,
    (fun Loop7(CanonParts, I) -> case (I < length(Binary1)) of true -> CanonParts1 = CanonParts ++ [lists:flatten(io_lib:format("~p", [binToInt(lists:sublist(Binary1, (I)+1, ((I + 8))-(I)))]))], I15 = (I + 8), Loop7(I15, CanonParts1); _ -> ok end end(CanonParts0, I14)),
    join(CanonParts1, ".") ++ "/" ++ lists:nth((1)+1, Parts).

main(_) ->
    Tests = ["87.70.141.1/22", "36.18.154.103/12", "62.62.197.11/29", "67.137.119.181/4", "161.214.74.21/24", "184.232.176.184/18"],
    lists:foreach(fun(T) -> io:format("~p~n", [padRight(T, 18) ++ " -> " ++ canonicalize(T)]) end, Tests).

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
