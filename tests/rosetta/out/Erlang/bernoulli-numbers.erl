% bernoulli-numbers.erl - generated from bernoulli-numbers.mochi

bernoulli(N) ->
    A0 = [],
    M0 = 0,
    (fun Loop1(A, M) -> case (M =< N) of true -> A1 = A ++ [(1 / (((M + 1))))], J0 = M, (fun Loop0(A, J) -> case (J >= 1) of true -> A2 = lists:sublist(A, (J - 1)) ++ [((J) * ((lists:nth(((J - 1))+1, A) - lists:nth((J)+1, A))))] ++ lists:nthtail(((J - 1))+1, A), J1 = (J - 1), Loop0(J1, A2); _ -> ok end end(A, J0)), M1 = (M + 1), Loop1(A2, M1); _ -> ok end end(A0, M0)),
    lists:nth((0)+1, A2).

main(_) ->
    lists:foreach(fun(I) -> B = bernoulli(I), (case (num(B) /= 0) of true -> NumStr = lists:flatten(io_lib:format("~p", [num(B)])), DenStr = lists:flatten(io_lib:format("~p", [denom(B)])), io:format("~p~n", ["B(" ++ mochi_get(padStart, lists:flatten(io_lib:format("~p", [I])))(2, " ") ++ ") =" ++ mochi_get(padStart, NumStr)(45, " ") ++ "/" ++ DenStr]); _ -> ok end) end, lists:seq(0, (61)-1)).

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
