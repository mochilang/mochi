% apply-a-digital-filter-direct-form-ii-transposed-.erl - generated from apply-a-digital-filter-direct-form-ii-transposed-.mochi

applyFilter(Input, A, B) ->
    Out0 = [],
    Scale = (1 / lists:nth((0)+1, A)),
    I0 = 0,
    (fun Loop2(J, Out, I) -> case (I < length(Input)) of true -> Tmp0 = 0, J0 = 0, (fun Loop0(Tmp, J) -> case ((J =< I) andalso (J < length(B))) of true -> Tmp1 = (Tmp + (lists:nth((J)+1, B) * lists:nth(((I - J))+1, Input))), J1 = (J + 1), Loop0(Tmp1, J1); _ -> ok end end(Tmp0, J)), J2 = 0, (fun Loop1(Tmp, J) -> case ((J < I) andalso ((J + 1) < length(A))) of true -> Tmp2 = (Tmp - (lists:nth(((J + 1))+1, A) * lists:nth((((I - J) - 1))+1, Out))), J3 = (J + 1), Loop1(Tmp2, J3); _ -> ok end end(Tmp1, J2)), Out1 = Out ++ [(Tmp2 * Scale)], I1 = (I + 1), Loop2(J3, Out1, I1); _ -> ok end end(J, Out0, I0)),
    Out1.

main(_) ->
    A = [1, -2.7756e-16, 0.33333333, -1.85e-17],
    B = [0.16666667, 0.5, 0.5, 0.16666667],
    Sig = [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589],
    Res = applyFilter(Sig, A, B),
    K0 = 0,
    (fun Loop3(K) -> case (K < length(Res)) of true -> io:format("~p~n", [mochi_get(K, Res)]), K1 = (K + 1), Loop3(K1); _ -> ok end end(K0)).

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
