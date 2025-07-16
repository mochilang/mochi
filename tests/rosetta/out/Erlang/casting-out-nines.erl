% casting-out-nines.erl - generated from casting-out-nines.mochi

parseIntBase(S, Base) ->
    N0 = 0,
    I0 = 0,
    (fun Loop1(N, I) -> case (I < length(S)) of true -> J0 = 0, V0 = 0, (fun Loop0(J) -> case (J < length(Digits)) of true -> (case (string:substr(Digits, (J)+1, ((J + 1))-(J)) == string:substr(S, (I)+1, ((I + 1))-(I))) of true -> V1 = J, throw(break); _ -> ok end), J1 = (J + 1), Loop0(J1); _ -> ok end end(J0)), N1 = ((N * Base) + V1), I1 = (I + 1), Loop1(I1, N1); _ -> ok end end(N0, I0)),
    N1.

intToBase(N, Base) ->
    (case (N1 == 0) of true -> "0"; _ -> ok end),
    Out0 = "",
    V2 = N1,
    (fun Loop2(V, Out) -> case (V > 0) of true -> D = rem(V, Base), Out1 = (lists:sublist(Digits, (D)+1, ((D + 1))-(D)) + Out), V3 = (V / Base), Loop2(Out1, V3); _ -> ok end end(V2, Out0)),
    Out1.

subset(Base, Begin, End) ->
    B0 = parseIntBase(Begin, Base),
    E0 = parseIntBase(End, Base),
    Out2 = [],
    K0 = B0,
    (fun Loop3(K) -> case (K =< E0) of true -> Ks = intToBase(K, Base), Mod = (Base - 1), R1 = rem(parseIntBase(Ks, Base), Mod), R2 = rem(((parseIntBase(Ks, Base) * parseIntBase(Ks, Base))), Mod), (case (R1 == R2) of true -> Out3 = Out2 ++ [Ks]; _ -> ok end), K1 = (K + 1), Loop3(K1); _ -> ok end end(K0)),
    Out3.

main(_) ->
    TestCases = [#{"base" => 10, "begin" => "1", "end" => "100", "kaprekar" => ["1", "9", "45", "55", "99"]}, #{"base" => 17, "begin" => "10", "end" => "gg", "kaprekar" => ["3d", "d4", "gg"]}],
    Idx0 = 0,
    (fun Loop6(Idx) -> case (Idx < length(TestCases)) of true -> Tc = lists:nth((Idx)+1, TestCases), io:format("~p~n", ["\nTest case base = " ++ lists:flatten(io_lib:format("~p", [mochi_get("base", Tc)])) ++ ", begin = " ++ mochi_get("begin", Tc) ++ ", end = " ++ mochi_get("end", Tc) ++ ":"]), S = subset(mochi_get("base", Tc), mochi_get("begin", Tc), mochi_get("end", Tc)), io:format("~p~n", ["Subset:  " ++ lists:flatten(io_lib:format("~p", [S]))]), io:format("~p~n", ["Kaprekar:" ++ lists:flatten(io_lib:format("~p", [mochi_get("kaprekar", Tc)]))]), Sx0 = 0, Valid0 = true, I2 = 0, (fun Loop5(I) -> case (I < length(mochi_get("kaprekar", Tc))) of true -> K = mochi_get(I, mochi_get("kaprekar", Tc)), Found0 = false, (fun Loop4(Sx) -> case (Sx < length(S)) of true -> (case (lists:nth((Sx)+1, S) == K) of true -> Found1 = true, Sx1 = (Sx + 1), throw(break); _ -> ok end), Sx2 = (Sx + 1), Loop4(Sx2); _ -> ok end end(Sx0)), (case not Found1 of true -> io:format("~p~n", ["Fail:" ++ K ++ " not in subset"]), Valid1 = false, throw(break); _ -> ok end), I3 = (I + 1), Loop5(I3); _ -> ok end end(I2)), (case Valid1 of undefined -> ok; false -> ok; _ -> io:format("~p~n", ["Valid subset."]) end), Idx1 = (Idx + 1), Loop6(Idx1); _ -> ok end end(Idx0)).

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
