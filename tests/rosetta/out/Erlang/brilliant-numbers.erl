% brilliant-numbers.erl - generated from brilliant-numbers.mochi

primesUpTo(N) ->
    Sieve0 = [],
    I0 = 0,
    (fun Loop0(Sieve, I) -> case (I =< N) of true -> Sieve1 = Sieve ++ [true], I1 = (I + 1), Loop0(Sieve1, I1); _ -> ok end end(Sieve0, I0)),
    P0 = 2,
    (fun Loop2(P) -> case ((P * P) =< N) of true -> (case lists:nth((P)+1, Sieve1) of undefined -> ok; false -> ok; _ -> M0 = (P * P), (fun Loop1(Sieve, M) -> case (M =< N) of true -> Sieve2 = lists:sublist(Sieve, M) ++ [false] ++ lists:nthtail((M)+1, Sieve), M1 = (M + P), Loop1(Sieve2, M1); _ -> ok end end(Sieve1, M0)) end), P1 = (P + 1), Loop2(P1); _ -> ok end end(P0)),
    Res0 = [],
    X0 = 2,
    (fun Loop3(X) -> case (X =< N) of true -> (case lists:nth((X)+1, Sieve2) of undefined -> ok; false -> ok; _ -> Res1 = Res0 ++ [X] end), X1 = (X + 1), Loop3(X1); _ -> ok end end(X0)),
    Res1.

sortInts(Xs) ->
    Res2 = [],
    Tmp0 = Xs,
    (fun Loop6(Res, Tmp) -> case (length(Tmp) > 0) of true -> Min0 = lists:nth((0)+1, Tmp), Idx0 = 0, I2 = 1, (fun Loop4(I) -> case (I < length(Tmp)) of true -> (case (mochi_get(I, Tmp) < Min0) of true -> Min1 = mochi_get(I, Tmp), Idx1 = I; _ -> ok end), I3 = (I + 1), Loop4(I3); _ -> ok end end(I2)), Res3 = Res ++ [Min1], Out0 = [], J0 = 0, (fun Loop5(J) -> case (J < length(Tmp)) of true -> (case (J /= Idx1) of true -> Out1 = Out0 ++ [mochi_get(J, Tmp)]; _ -> ok end), J1 = (J + 1), Loop5(J1); _ -> ok end end(J0)), Tmp1 = Out1, Loop6(Res3, Tmp1); _ -> ok end end(Res2, Tmp0)),
    Res3.

commatize(N) ->
    S0 = lists:flatten(io_lib:format("~p", [N])),
    I4 = (length(S0) - 3),
    (fun Loop7(S, I) -> case (I >= 1) of true -> S1 = lists:sublist(S, (0)+1, (I)-(0)) ++ "," ++ lists:sublist(S, (I)+1, (length(S))-(I)), I5 = (I - 3), Loop7(S1, I5); _ -> ok end end(S0, I4)),
    S1.

getBrilliant(Digits, Limit, CountOnly) ->
    Brilliant0 = [],
    Count0 = 0,
    Pow0 = 1,
    Next0 = 999999999999999,
    K0 = 1,
    (fun Loop10(Pow, K) -> case (K =< Digits) of true -> S2 = [], try lists:foreach(fun(P) -> try (case (P1 >= (Pow * 10)) of true -> throw(break); _ -> ok end), (case (P1 > Pow) of true -> S3 = S2 ++ [P1]; _ -> ok end) catch throw:continue -> ok end end, Primes) catch throw:break -> ok end, I6 = 0, (fun Loop9(I) -> case (I < length(S3)) of true -> J2 = I, (fun Loop8(J) -> case (J < length(S3)) of true -> Prod0 = (lists:nth((I)+1, S3) * lists:nth((J)+1, S3)), (case (Prod0 < Limit) of true -> (case CountOnly of undefined -> Brilliant1 = Brilliant0 ++ [Prod0]; false -> Brilliant1 = Brilliant0 ++ [Prod0]; _ -> Count1 = (Count0 + 1) end); _ -> (case (Prod0 < Next0) of true -> Next1 = Prod0; _ -> ok end), throw(break) end), J3 = (J + 1), Loop8(J3); _ -> ok end end(J2)), I7 = (I + 1), Loop9(I7); _ -> ok end end(I6)), Pow1 = (Pow * 10), K1 = (K + 1), Loop10(K1, Pow1); _ -> ok end end(Pow0, K0)),
    (case CountOnly of undefined -> ok; false -> ok; _ -> #{"bc" => Count1, "next" => Next1} end),
    #{"bc" => Brilliant1, "next" => Next1}.

main() ->
    io:format("~p~n", ["First 100 brilliant numbers:"]),
    R = getBrilliant(2, 10000, false),
    Br0 = sortInts(mochi_get("bc", R)),
    Br1 = lists:sublist(Br0, (0)+1, (100)-(0)),
    I8 = 0,
    (fun Loop11(I) -> case (I < length(Br1)) of true -> io:format("~p ~p~n", [mochi_get(padStart, lists:flatten(io_lib:format("~p", [mochi_get(I, Br1)])))(4, " ") ++ " ", false]), (case (rem(((I + 1)), 10) == 0) of true -> io:format("~p ~p~n", ["", true]); _ -> ok end), I9 = (I + 1), Loop11(I9); _ -> ok end end(I8)),
    io:format("~p ~p~n", ["", true]),
    K2 = 1,
    (fun Loop12(K) -> case (K =< 13) of true -> Limit = Pow1(10, K), R2 = getBrilliant(K, Limit, true), Total = mochi_get("bc", R2), Next = mochi_get("next", R2), Climit = commatize(Limit), Ctotal = commatize((Total + 1)), Cnext = commatize(Next), io:format("~p~n", ["First >= " ++ mochi_get(padStart, Climit)(18, " ") ++ " is " ++ mochi_get(padStart, Ctotal)(14, " ") ++ " in the series: " ++ mochi_get(padStart, Cnext)(18, " ")]), K3 = (K + 1), Loop12(K3); _ -> ok end end(K2)).

main(_) ->
    Primes0 = primesUpTo(3200000).

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
