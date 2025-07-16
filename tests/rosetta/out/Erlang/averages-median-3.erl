% averages-median-3.erl - generated from averages-median-3.mochi

qsel(A, K) ->
    Arr0 = A,
    (fun Loop1(Arr, Px) -> case (length(Arr) > 1) of true -> Px0 = rem(now(), length(Arr)), Pv0 = mochi_get(Px, Arr), Last = (length(Arr) - 1), Tmp = mochi_get(Px, Arr), Arr1 = maps:put(Px, mochi_get(Last, Arr), Arr), Arr2 = maps:put(Last, Tmp, Arr), Px1 = 0, I0 = 0, (fun Loop0(I) -> case (I < Last) of true -> V = mochi_get(I, Arr), (case (V < Pv0) of true -> Tmp2 = mochi_get(Px, Arr), Arr3 = maps:put(Px, mochi_get(I, Arr), Arr), Arr4 = maps:put(I, Tmp2, Arr), Px2 = (Px + 1); _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)), (case (Px == K) of true -> Pv0; _ -> ok end), (case (K < Px) of true -> Arr5 = lists:sublist(Arr, (0)+1, (Px)-(0)); _ -> Tmp2 = mochi_get(Px, Arr), Arr6 = maps:put(Px, Pv0, Arr), Arr7 = maps:put(Last, Tmp2, Arr), Arr8 = lists:sublist(Arr, (((Px + 1)))+1, (length(Arr))-(((Px + 1)))), K0 = (K - ((Px + 1))) end), Loop1(Arr8, Px2); _ -> ok end end(Arr0, Px)),
    lists:nth((0)+1, Arr8).

median(List) ->
    Arr9 = List,
    Half = ((length(Arr9) / 2)),
    Med = qsel(Arr9, Half),
    (case (rem(length(Arr9), 2) == 0) of true -> (((Med + qsel(Arr9, (Half - 1)))) / 2); _ -> ok end),
    Med.

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
