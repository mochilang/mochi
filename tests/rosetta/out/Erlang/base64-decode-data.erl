% base64-decode-data.erl - generated from base64-decode-data.mochi

indexOf(S, Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(S)) of true -> (case (lists:nth((I)+1, S) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    -1.

parseIntStr(Str) ->
    I2 = 0,
    Neg0 = false,
    (case ((length(Str) > 0) andalso (lists:nth((0)+1, Str) == "-")) of true -> Neg1 = true, I3 = 1; _ -> ok end),
    N0 = 0,
    Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
    (fun Loop1(N, I) -> case (I < length(Str)) of true -> N1 = ((N * 10) + mochi_get(lists:nth((I)+1, Str), Digits)), I4 = (I + 1), Loop1(N1, I4); _ -> ok end end(N0, I3)),
    (case Neg1 of undefined -> ok; false -> ok; _ -> N2 = -N1 end),
    N2.

ord(Ch) ->
    Idx0 = indexOf(Upper, Ch),
    (case (Idx0 >= 0) of true -> (65 + Idx0); _ -> ok end),
    Idx1 = indexOf(Lower, Ch),
    (case (Idx1 >= 0) of true -> (97 + Idx1); _ -> ok end),
    (case ((Ch >= "0") andalso (Ch =< "9")) of true -> (48 + parseIntStr(Ch)); _ -> ok end),
    (case (Ch == "+") of true -> 43; _ -> ok end),
    (case (Ch == "/") of true -> 47; _ -> ok end),
    (case (Ch == " ") of true -> 32; _ -> ok end),
    (case (Ch == "=") of true -> 61; _ -> ok end),
    0.

chr(N) ->
    (case ((N2 >= 65) andalso (N2 < 91)) of true -> lists:sublist(Upper, ((N2 - 65))+1, ((N2 - 64))-((N2 - 65))); _ -> ok end),
    (case ((N2 >= 97) andalso (N2 < 123)) of true -> lists:sublist(Lower, ((N2 - 97))+1, ((N2 - 96))-((N2 - 97))); _ -> ok end),
    (case ((N2 >= 48) andalso (N2 < 58)) of true -> , lists:sublist(Digits, ((N2 - 48))+1, ((N2 - 47))-((N2 - 48))); _ -> ok end),
    (case (N2 == 43) of true -> "+"; _ -> ok end),
    (case (N2 == 47) of true -> "/"; _ -> ok end),
    (case (N2 == 32) of true -> " "; _ -> ok end),
    (case (N2 == 61) of true -> "="; _ -> ok end),
    "?".

toBinary(N, Bits) ->
    B0 = "",
    Val0 = N2,
    I5 = 0,
    (fun Loop2(Val, I, B) -> case (I < Bits) of true -> B1 = (lists:flatten(io_lib:format("~p", [rem(Val, 2)])) + B), Val1 = ((Val / 2)), I6 = (I + 1), Loop2(B1, Val1, I6); _ -> ok end end(Val0, I5, B0)),
    B1.

binToInt(Bits) ->
    N3 = 0,
    I7 = 0,
    (fun Loop3(N, I) -> case (I < length(Bits)) of true -> N4 = ((N * 2) + parseIntStr(string:substr(Bits, (I)+1, ((I + 1))-(I)))), I8 = (I + 1), Loop3(N4, I8); _ -> ok end end(N3, I7)),
    N4.

base64Encode(Text) ->
    Bin0 = "",
    {Bin1} = lists:foldl(fun(Ch, {Bin}) -> Bin1 = (Bin + toBinary(ord(Ch), 8)), {Bin1} end, {Bin0}, Text),
    (fun Loop4(Bin) -> case (rem(length(Bin), 6) /= 0) of true -> Bin2 = Bin ++ "0", Loop4(Bin2); _ -> ok end end(Bin1)),
    Out0 = "",
    I9 = 0,
    (fun Loop5(Out, I) -> case (I < length(Bin2)) of true -> Chunk = string:substr(Bin2, (I)+1, ((I + 6))-(I)), Val = binToInt(Chunk), Out1 = (Out + lists:sublist(Alphabet, (Val)+1, ((Val + 1))-(Val))), I10 = (I + 6), Loop5(Out1, I10); _ -> ok end end(Out0, I9)),
    Pad = rem(((3 - (rem(length(Text), 3)))), 3),
    (case (Pad == 1) of true -> Out2 = string:substr(Out1, (0)+1, ((length(Out1) - 1))-(0)) ++ "="; _ -> ok end),
    (case (Pad == 2) of true -> Out3 = string:substr(Out2, (0)+1, ((length(Out2) - 2))-(0)) ++ "=="; _ -> ok end),
    Out3.

base64Decode(Enc) ->
    Bin3 = "",
    I11 = 0,
    (fun Loop6(Bin, I) -> case (I < length(Enc)) of true -> Ch = lists:nth((I)+1, Enc), (case (Ch == "=") of true -> throw(break); _ -> ok end), Idx = indexOf(Alphabet, Ch), Bin4 = (Bin + toBinary(Idx, 6)), I12 = (I + 1), Loop6(Bin4, I12); _ -> ok end end(Bin3, I11)),
    Out4 = "",
    I13 = 0,
    (fun Loop7(Out, I) -> case ((I + 8) =< length(Bin4)) of true -> Chunk = string:substr(Bin4, (I)+1, ((I + 8))-(I)), Val = binToInt(Chunk), Out5 = (Out + chr(Val)), I14 = (I + 8), Loop7(Out5, I14); _ -> ok end end(Out4, I13)),
    Out5.

main(_) ->
    io:format("~p~n", ["Original : " ++ "Rosetta Code Base64 decode data task"]),
    Enc = base64Encode("Rosetta Code Base64 decode data task"),
    io:format("~p~n", ["\nEncoded  : " ++ Enc]),
    Dec = base64Decode(Enc),
    io:format("~p~n", ["\nDecoded  : " ++ Dec]).

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
