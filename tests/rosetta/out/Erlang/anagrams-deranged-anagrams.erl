% anagrams-deranged-anagrams.erl - generated from anagrams-deranged-anagrams.mochi

sortRunes(S) ->
    Arr0 = [],
    I0 = 0,
    (fun Loop0(I, Arr) -> case (I < length(S)) of true -> Arr1 = Arr ++ [string:substr(S, (I)+1, ((I + 1))-(I))], I1 = (I + 1), Loop0(I1, Arr1); _ -> ok end end(I0, Arr0)),
    N0 = length(Arr1),
    M0 = 0,
    (fun Loop2(M) -> case (M < N0) of true -> J0 = 0, (fun Loop1(J) -> case (J < (N0 - 1)) of true -> (case (lists:nth((J)+1, Arr1) > lists:nth(((J + 1))+1, Arr1)) of true -> Tmp = lists:nth((J)+1, Arr1), Arr2 = lists:sublist(Arr1, J) ++ [lists:nth(((J + 1))+1, Arr1)] ++ lists:nthtail((J)+1, Arr1), Arr3 = lists:sublist(Arr2, (J + 1)) ++ [Tmp] ++ lists:nthtail(((J + 1))+1, Arr2); _ -> ok end), J1 = (J + 1), Loop1(J1); _ -> ok end end(J0)), M1 = (M + 1), Loop2(M1); _ -> ok end end(M0)),
    Out0 = "",
    I2 = 0,
    (fun Loop3(Out, I) -> case (I < N0) of true -> Out1 = (Out + lists:nth((I)+1, Arr3)), I3 = (I + 1), Loop3(I3, Out1); _ -> ok end end(Out0, I2)),
    Out1.

deranged(A, B) ->
    (case (length(A) /= length(B)) of true -> false; _ -> ok end),
    I4 = 0,
    (fun Loop4(I) -> case (I < length(A)) of true -> (case (string:substr(A, (I)+1, ((I + 1))-(I)) == string:substr(B, (I)+1, ((I + 1))-(I))) of true -> false; _ -> ok end), I5 = (I + 1), Loop4(I5); _ -> ok end end(I4)),
    true.

main() ->
    Words = ["constitutionalism", "misconstitutional"],
    M2 = #{},
    BestLen0 = 0,
    W10 = "",
    W20 = "",
    try {M4} = lists:foldl(fun(W, {M}) -> try (case (length(W) =< BestLen0) of true -> throw(continue); _ -> ok end), K = sortRunes(W), (case not (maps:is_key(K, M)) of true -> M3 = maps:put(K, [W], M), throw(continue); _ -> ok end), try lists:foreach(fun({C,_}) -> try (case deranged(W, C) of undefined -> ok; false -> ok; _ -> BestLen1 = length(W), W11 = C, W21 = W, throw(break) end) catch throw:continue -> ok end end, maps:to_list(mochi_get(K, M))) catch throw:break -> ok end, M4 = maps:put(K, mochi_get(K, M) ++ [W], M) catch throw:continue -> {M4} end, {M4} end, {M2}, Words) catch throw:break -> ok end,
    io:format("~p~n", [W11 ++ " " ++ W21 ++ " : Length " ++ lists:flatten(io_lib:format("~p", [BestLen1]))]).

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
