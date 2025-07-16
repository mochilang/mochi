% averages-median-2.erl - generated from averages-median-2.mochi

sel(List, K) ->
    I0 = 0,
    (fun Loop1(List, I) -> case (I =< K) of true -> MinIndex0 = I, J0 = (I + 1), (fun Loop0(J) -> case (J < length(List)) of true -> (case (lists:nth((J)+1, List) < lists:nth((MinIndex0)+1, List)) of true -> MinIndex1 = J; _ -> ok end), J1 = (J + 1), Loop0(J1); _ -> ok end end(J0)), Tmp = lists:nth((I)+1, List), List0 = lists:sublist(List, I) ++ [lists:nth((MinIndex1)+1, List)] ++ lists:nthtail((I)+1, List), List1 = lists:sublist(List, MinIndex1) ++ [Tmp] ++ lists:nthtail((MinIndex1)+1, List), I1 = (I + 1), Loop1(List1, I1); _ -> ok end end(List, I0)),
    lists:nth((K)+1, List1).

median(A) ->
    Arr0 = A,
    Half = ((length(Arr0) / 2)),
    Med = sel(Arr0, Half),
    (case (rem(length(Arr0), 2) == 0) of true -> (((Med + mochi_get((Half - 1), Arr0))) / 2); _ -> ok end),
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
