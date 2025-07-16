% averages-median-1.erl - generated from averages-median-1.mochi

sortFloat(Xs) ->
    Arr0 = Xs,
    N0 = length(Arr0),
    I0 = 0,
    (fun Loop1(I) -> case (I < N0) of true -> J0 = 0, (fun Loop0(J) -> case (J < (N0 - 1)) of true -> (case (mochi_get(J, Arr0) > mochi_get((J + 1), Arr0)) of true -> Tmp = mochi_get(J, Arr0), Arr1 = maps:put(J, mochi_get((J + 1), Arr0), Arr0), Arr2 = maps:put((J + 1), Tmp, Arr1); _ -> ok end), J1 = (J + 1), Loop0(J1); _ -> ok end end(J0)), I1 = (I + 1), Loop1(I1); _ -> ok end end(I0)),
    Arr2.

median(A) ->
    Arr3 = sortFloat(A),
    Half = ((length(Arr3) / 2)),
    M0 = mochi_get(Half, Arr3),
    (case (rem(length(Arr3), 2) == 0) of true -> M1 = (((M0 + mochi_get((Half - 1), Arr3))) / 2); _ -> ok end),
    M1.

main(_) ->
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [median([3, 1, 4, 1])]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [median([3, 1, 4, 1, 5])]))]).

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
