% bifid-cipher.erl - generated from bifid-cipher.mochi

square_to_maps(Square) ->
    Emap0 = #{},
    Dmap0 = #{},
    X0 = 0,
    (fun Loop1(X) -> case (X < length(Square)) of true -> Row = lists:nth((X)+1, Square), Y0 = 0, (fun Loop0(Emap, Dmap, Y) -> case (Y < length(Row)) of true -> Ch = mochi_get(Y, Row), Emap1 = maps:put(Ch, [X, Y], Emap), Dmap1 = maps:put(lists:flatten(io_lib:format("~p", [X])) ++ "," ++ lists:flatten(io_lib:format("~p", [Y])), Ch, Dmap), Y1 = (Y + 1), Loop0(Emap1, Dmap1, Y1); _ -> ok end end(Emap0, Dmap0, Y0)), X1 = (X + 1), Loop1(X1); _ -> ok end end(X0)),
    #{"e" => Emap1, "d" => Dmap1}.

remove_space(Text, Emap) ->
    S = upper(Text),
    Out0 = "",
    I0 = 0,
    (fun Loop2(I) -> case (I < length(S)) of true -> Ch = lists:sublist(S, (I)+1, ((I + 1))-(I)), (case ((Ch /= " ") andalso maps:is_key(Ch, Emap1)) of true -> Out1 = (Out0 + Ch); _ -> ok end), I1 = (I + 1), Loop2(I1); _ -> ok end end(I0)),
    Out1.

encrypt(Text, Emap, Dmap) ->
    Text0 = remove_space(Text, Emap1),
    Row00 = [],
    Row10 = [],
    I2 = 0,
    (fun Loop3(Row0, Row1, I) -> case (I < length(Text0)) of true -> Ch = string:substr(Text0, (I)+1, ((I + 1))-(I)), Xy = mochi_get(Ch, Emap1), Row01 = Row0 ++ [lists:nth((0)+1, Xy)], Row11 = Row1 ++ [lists:nth((1)+1, Xy)], I3 = (I + 1), Loop3(Row01, Row11, I3); _ -> ok end end(Row00, Row10, I2)),
    {Row02} = lists:foldl(fun(V, {Row0}) -> Row02 = Row0 ++ [V], {Row02} end, {Row01}, Row11),
    Res0 = "",
    J0 = 0,
    (fun Loop4(Res, J) -> case (J < length(Row02)) of true -> Key = lists:flatten(io_lib:format("~p", [lists:nth((J)+1, Row02)])) ++ "," ++ lists:flatten(io_lib:format("~p", [lists:nth(((J + 1))+1, Row02)])), Res1 = (Res + mochi_get(Key, Dmap1)), J1 = (J + 2), Loop4(Res1, J1); _ -> ok end end(Res0, J0)),
    Res1.

decrypt(Text, Emap, Dmap) ->
    Text1 = remove_space(Text0, Emap1),
    Coords0 = [],
    I4 = 0,
    (fun Loop5(Coords, I) -> case (I < length(Text1)) of true -> Ch = string:substr(Text1, (I)+1, ((I + 1))-(I)), Xy = mochi_get(Ch, Emap1), Coords1 = Coords ++ [lists:nth((0)+1, Xy)], Coords2 = Coords ++ [lists:nth((1)+1, Xy)], I5 = (I + 1), Loop5(I5, Coords2); _ -> ok end end(Coords0, I4)),
    Half0 = (length(Coords2) / 2),
    K10 = [],
    K20 = [],
    Idx0 = 0,
    (fun Loop6(K1, Idx) -> case (Idx < Half0) of true -> K11 = K1 ++ [lists:nth((Idx)+1, Coords2)], Idx1 = (Idx + 1), Loop6(K11, Idx1); _ -> ok end end(K10, Idx0)),
    (fun Loop7(K2, Idx) -> case (Idx < length(Coords2)) of true -> K21 = K2 ++ [lists:nth((Idx)+1, Coords2)], Idx2 = (Idx + 1), Loop7(K21, Idx2); _ -> ok end end(K20, Idx1)),
    Res2 = "",
    J2 = 0,
    (fun Loop8(Res, J) -> case (J < Half0) of true -> Key = lists:flatten(io_lib:format("~p", [lists:nth((J)+1, K11)])) ++ "," ++ lists:flatten(io_lib:format("~p", [lists:nth((J)+1, K21)])), Res3 = (Res + mochi_get(Key, Dmap1)), J3 = (J + 1), Loop8(Res3, J3); _ -> ok end end(Res2, J2)),
    Res3.

main() ->
    SquareRosetta = [["A", "B", "C", "D", "E"], ["F", "G", "H", "I", "K"], ["L", "M", "N", "O", "P"], ["Q", "R", "S", "T", "U"], ["V", "W", "X", "Y", "Z"], ["J", "1", "2", "3", "4"]],
    SquareWikipedia = [["B", "G", "W", "K", "Z"], ["Q", "P", "N", "D", "S"], ["I", "O", "A", "X", "E"], ["F", "C", "L", "U", "M"], ["T", "H", "Y", "V", "R"], ["J", "1", "2", "3", "4"]],
    Maps0 = square_to_maps(SquareRosetta),
    Emap2 = mochi_get("e", Maps0),
    Dmap2 = mochi_get("d", Maps0),
    io:format("~p~n", ["from Rosettacode"]),
    io:format("~p~n", ["original:\t " ++ TextRosetta]),
    S0 = encrypt(TextRosetta, Emap2, Dmap2),
    io:format("~p~n", ["codiert:\t " ++ S]),
    S1 = decrypt(S, Emap2, Dmap2),
    io:format("~p~n", ["and back:\t " ++ S]),
    Maps1 = square_to_maps(SquareWikipedia),
    Emap3 = mochi_get("e", Maps1),
    Dmap3 = mochi_get("d", Maps1),
    io:format("~p~n", ["from Wikipedia"]),
    io:format("~p~n", ["original:\t " ++ TextWikipedia]),
    S2 = encrypt(TextWikipedia, Emap3, Dmap3),
    io:format("~p~n", ["codiert:\t " ++ S]),
    S3 = decrypt(S, Emap3, Dmap3),
    io:format("~p~n", ["and back:\t " ++ S]),
    Maps2 = square_to_maps(SquareWikipedia),
    Emap4 = mochi_get("e", Maps2),
    Dmap4 = mochi_get("d", Maps2),
    io:format("~p~n", ["from Rosettacode long part"]),
    io:format("~p~n", ["original:\t " ++ TextTest]),
    S4 = encrypt(TextTest, Emap4, Dmap4),
    io:format("~p~n", ["codiert:\t " ++ S]),
    S5 = decrypt(S, Emap4, Dmap4),
    io:format("~p~n", ["and back:\t " ++ S]).

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
