% csv-data-manipulation.erl - generated from csv-data-manipulation.mochi

join(Xs, Sep) ->
    Res0 = "",
    I0 = 0,
    (fun Loop0(Res, I) -> case (I < length(Xs)) of true -> (case (I > 0) of true -> Res1 = (Res + Sep); _ -> ok end), Res2 = (Res + lists:nth((I)+1, Xs)), I1 = (I + 1), Loop0(Res2, I1); _ -> ok end end(Res0, I0)),
    Res2.

parseIntStr(Str) ->
    I2 = 0,
    Neg0 = false,
    (case ((length(Str) > 0) andalso (string:substr(Str, (0)+1, (1)-(0)) == "-")) of true -> Neg1 = true, I3 = 1; _ -> ok end),
    N0 = 0,
    Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
    (fun Loop1(N, I) -> case (I < length(Str)) of true -> N1 = ((N * 10) + mochi_get(string:substr(Str, (I)+1, ((I + 1))-(I)), Digits)), I4 = (I + 1), Loop1(N1, I4); _ -> ok end end(N0, I3)),
    (case Neg1 of undefined -> ok; false -> ok; _ -> N2 = -N1 end),
    N2.

main(_) ->
    Rows0 = [["A", "B", "C"], ["1", "2", "3"], ["4", "5", "6"], ["7", "8", "9"]],
    Rows1 = lists:sublist(Rows0, 0) ++ [lists:nth((0)+1, Rows0) ++ ["SUM"]] ++ lists:nthtail((0)+1, Rows0),
    I5 = 1,
    (fun Loop2(Rows, I) -> case (I < length(Rows)) of true -> Sum0 = 0, {Sum1} = lists:foldl(fun(S, {Sum}) -> Sum1 = (Sum + parseIntStr(S)), {Sum1} end, {Sum0}, lists:nth((I)+1, Rows)), Rows2 = lists:sublist(Rows, I) ++ [lists:nth((I)+1, Rows) ++ [lists:flatten(io_lib:format("~p", [Sum1]))]] ++ lists:nthtail((I)+1, Rows), I6 = (I + 1), Loop2(Rows2, I6); _ -> ok end end(Rows1, I5)),
    lists:foreach(fun(R) -> io:format("~p~n", [join(R, ",")]) end, Rows2).

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
