% amb.erl - generated from amb.mochi

amb(Wordsets, Res, Idx) ->
    (case (Idx == length(Wordsets)) of true -> true; _ -> ok end),
    Prev0 = "",
    (case (Idx > 0) of true -> Prev1 = lists:nth(((Idx - 1))+1, Res); _ -> ok end),
    I0 = 0,
    (fun Loop0(I) -> case (I < length(lists:nth((Idx)+1, Wordsets))) of true -> W = mochi_get(I, lists:nth((Idx)+1, Wordsets)), (case ((Idx == 0) orelse (string:substr(Prev1, ((length(Prev1) - 1))+1, (length(Prev1))-((length(Prev1) - 1))) == string:substr(W, (0)+1, (1)-(0)))) of true -> Res0 = lists:sublist(Res, Idx) ++ [W] ++ lists:nthtail((Idx)+1, Res), (case amb(Wordsets, Res0, (Idx + 1)) of undefined -> ok; false -> ok; _ -> true end); _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    false.

main() ->
    Wordset = [["the", "that", "a"], ["frog", "elephant", "thing"], ["walked", "treaded", "grows"], ["slowly", "quickly"]],
    Res1 = [],
    I2 = 0,
    (fun Loop1(Res, I) -> case (I < length(Wordset)) of true -> Res2 = Res ++ [""], I3 = (I + 1), Loop1(Res2, I3); _ -> ok end end(Res1, I2)),
    (case amb(Wordset, Res2, 0) of undefined -> io:format("~p~n", ["No amb found"]); false -> io:format("~p~n", ["No amb found"]); _ -> Out0 = "[" ++ lists:nth((0)+1, Res2), J0 = 1, (fun Loop2(Out, J) -> case (J < length(Res2)) of true -> Out1 = Out ++ " " ++ lists:nth((J)+1, Res2), J1 = (J + 1), Loop2(Out1, J1); _ -> ok end end(Out0, J0)), Out2 = Out1 ++ "]", io:format("~p~n", [Out2]) end).

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
