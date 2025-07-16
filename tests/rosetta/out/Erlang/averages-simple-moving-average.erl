% averages-simple-moving-average.erl - generated from averages-simple-moving-average.mochi

indexOf(S, Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    -1.

fmt3(X) ->
    Y0 = (((((X * 1000)) + 0.5)) / 1000),
    S0 = lists:flatten(io_lib:format("~p", [Y0])),
    Dot0 = indexOf(S0, "."),
    (case (Dot0 == (0 - 1)) of true -> S1 = S0 ++ ".000"; _ -> Decs0 = ((length(S1) - Dot0) - 1), (case (Decs0 > 3) of true -> S2 = string:substr(S1, (0)+1, ((Dot0 + 4))-(0)); _ -> (fun Loop1(S, Decs) -> case (Decs < 3) of true -> S3 = S ++ "0", Decs1 = (Decs + 1), Loop1(S3, Decs1); _ -> ok end end(S2, Decs0)) end) end),
    S3.

pad(S, Width) ->
    Out0 = S3,
    (fun Loop2(Out) -> case (length(Out) < Width) of true -> Out1 = " " ++ Out, Loop2(Out1); _ -> ok end end(Out0)),
    Out1.

smaSeries(Xs, Period) ->
    Res0 = [],
    Sum0 = 0,
    I2 = 0,
    (fun Loop3(Sum, Res, I) -> case (I < length(Xs)) of true -> Sum1 = (Sum + lists:nth((I)+1, Xs)), (case (I >= Period) of true -> Sum2 = (Sum - lists:nth(((I - Period))+1, Xs)); _ -> ok end), Denom0 = (I + 1), (case (Denom0 > Period) of true -> Denom1 = Period; _ -> ok end), Res1 = Res ++ [(Sum / (Denom1))], I3 = (I + 1), Loop3(Sum2, Res1, I3); _ -> ok end end(Sum0, Res0, I2)),
    Res1.

main() ->
    Xs0 = [1, 2, 3, 4, 5, 5, 4, 3, 2, 1],
    Sma30 = smaSeries(Xs0, 3),
    Sma50 = smaSeries(Xs0, 5),
    io:format("~p~n", ["x       sma3   sma5"]),
    I4 = 0,
    (fun Loop4(I) -> case (I < length(Xs0)) of true -> Line = pad(fmt3(lists:nth((I)+1, Xs0)), 5) ++ "  " ++ pad(fmt3(mochi_get(I, Sma30)), 5) ++ "  " ++ pad(fmt3(mochi_get(I, Sma50)), 5), io:format("~p~n", [Line]), I5 = (I + 1), Loop4(I5); _ -> ok end end(I4)).

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
