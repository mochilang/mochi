% anagrams.erl - generated from anagrams.mochi

sortRunes(S) ->
    Arr0 = [],
    I0 = 0,
    (fun Loop0(Arr, I) -> case (I < length(S)) of true -> Arr1 = Arr ++ [string:substr(S, (I)+1, ((I + 1))-(I))], I1 = (I + 1), Loop0(Arr1, I1); _ -> ok end end(Arr0, I0)),
    N0 = length(Arr1),
    M0 = 0,
    (fun Loop2(M) -> case (M < N0) of true -> J0 = 0, (fun Loop1(J) -> case (J < (N0 - 1)) of true -> (case (lists:nth((J)+1, Arr1) > lists:nth(((J + 1))+1, Arr1)) of true -> Tmp = lists:nth((J)+1, Arr1), Arr2 = lists:sublist(Arr1, J) ++ [lists:nth(((J + 1))+1, Arr1)] ++ lists:nthtail((J)+1, Arr1), Arr3 = lists:sublist(Arr2, (J + 1)) ++ [Tmp] ++ lists:nthtail(((J + 1))+1, Arr2); _ -> ok end), J1 = (J + 1), Loop1(J1); _ -> ok end end(J0)), M1 = (M + 1), Loop2(M1); _ -> ok end end(M0)),
    Out0 = "",
    I2 = 0,
    (fun Loop3(Out, I) -> case (I < N0) of true -> Out1 = (Out + lists:nth((I)+1, Arr3)), I3 = (I + 1), Loop3(Out1, I3); _ -> ok end end(Out0, I2)),
    Out1.

sortStrings(Xs) ->
    Res0 = [],
    Tmp0 = Xs,
    (fun Loop6(Res, Tmp) -> case (length(Tmp) > 0) of true -> Min0 = lists:nth((0)+1, Tmp), Idx0 = 0, I4 = 1, (fun Loop4(I) -> case (I < length(Tmp)) of true -> (case (mochi_get(I, Tmp) < Min0) of true -> Min1 = mochi_get(I, Tmp), Idx1 = I; _ -> ok end), I5 = (I + 1), Loop4(I5); _ -> ok end end(I4)), Res1 = Res ++ [Min1], Out2 = [], J2 = 0, (fun Loop5(J) -> case (J < length(Tmp)) of true -> (case (J /= Idx1) of true -> Out3 = Out2 ++ [mochi_get(J, Tmp)]; _ -> ok end), J3 = (J + 1), Loop5(J3); _ -> ok end end(J2)), Tmp1 = Out3, Loop6(Res1, Tmp1); _ -> ok end end(Res0, Tmp)),
    Res1.

main() ->
    Words = ["abel", "able", "bale", "bela", "elba", "alger", "glare", "lager", "large", "regal", "angel", "angle", "galen", "glean", "lange", "caret", "carte", "cater", "crate", "trace", "elan", "lane", "lean", "lena", "neal", "evil", "levi", "live", "veil", "vile"],
    Groups0 = #{},
    MaxLen0 = 0,
    lists:foreach(fun(W) -> K = sortRunes(W), (case not (maps:is_key(K, Groups0)) of true -> Groups1 = maps:put(K, [W], Groups0); _ -> Groups2 = maps:put(K, mochi_get(K, Groups1) ++ [W], Groups1) end), (case (length(mochi_get(K, Groups2)) > MaxLen0) of true -> MaxLen1 = length(mochi_get(K, Groups2)); _ -> ok end) end, Words),
    Printed0 = #{},
    lists:foreach(fun(W) -> K = sortRunes(W), (case (length(mochi_get(K, Groups2)) == MaxLen1) of true -> (case not (maps:is_key(K, Printed0)) of true -> G0 = sortStrings(mochi_get(K, Groups2)), Line0 = "[" ++ lists:nth((0)+1, G0), I6 = 1, (fun Loop7(Line, I) -> case (I < length(G0)) of true -> Line1 = Line ++ " " ++ mochi_get(I, G0), I7 = (I + 1), Loop7(Line1, I7); _ -> ok end end(Line0, I6)), Line2 = Line1 ++ "]", io:format("~p~n", [Line2]), Printed1 = maps:put(K, true, Printed0); _ -> ok end); _ -> ok end) end, Words).

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
