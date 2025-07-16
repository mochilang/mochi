% balanced-ternary.erl - generated from balanced-ternary.mochi

trimLeftZeros(S) ->
    I0 = 0,
    (fun Loop0(I) -> case ((I < length(S)) andalso (string:substr(S, (I)+1, ((I + 1))-(I)) == "0")) of true -> I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    string:substr(S, (I1)+1, (length(S))-(I1)).

btString(S) ->
    S0 = trimLeftZeros(S),
    B0 = [],
    I2 = (length(S0) - 1),
    (fun Loop1(I) -> case (I >= 0) of true -> Ch = string:substr(S0, (I)+1, ((I + 1))-(I)), (case (Ch == "+") of true -> B1 = B0 ++ [1]; _ -> (case (Ch == "0") of true -> B2 = B1 ++ [0]; _ -> (case (Ch == "-") of true -> B3 = B2 ++ [(0 - 1)]; _ -> #{"bt" => [], "ok" => false} end) end) end), I3 = (I - 1), Loop1(I3); _ -> ok end end(I2)),
    #{"bt" => B3, "ok" => true}.

btToString(B) ->
    (case (length(B3) == 0) of true -> "0"; _ -> ok end),
    R0 = "",
    I4 = (length(B3) - 1),
    (fun Loop2(I) -> case (I >= 0) of true -> D = lists:nth((I)+1, B3), (case (D == (0 - 1)) of true -> R1 = R0 ++ "-"; _ -> (case (D == 0) of true -> R2 = R1 ++ "0"; _ -> R3 = R2 ++ "+" end) end), I5 = (I - 1), Loop2(I5); _ -> ok end end(I4)),
    R3.

btInt(I) ->
    (case (I5 == 0) of true -> []; _ -> ok end),
    N0 = I5,
    B4 = [],
    (fun Loop3(N, B) -> case (N /= 0) of true -> M0 = rem(N, 3), N1 = ((N / 3)), (case (M0 == 2) of true -> M1 = (0 - 1), N2 = (N + 1); _ -> (case (M1 == (0 - 2)) of true -> M2 = 1, N3 = (N - 1); _ -> ok end) end), B5 = B ++ [M2], Loop3(N3, B5); _ -> ok end end(N0, B4)),
    B5.

btToInt(B) ->
    R4 = 0,
    Pt0 = 1,
    I6 = 0,
    (fun Loop4(Pt, I, R) -> case (I < length(B5)) of true -> R5 = (R + (lists:nth((I)+1, B5) * Pt)), Pt1 = (Pt * 3), I7 = (I + 1), Loop4(R5, Pt1, I7); _ -> ok end end(Pt0, I6, R4)),
    R5.

btNeg(B) ->
    R6 = [],
    I8 = 0,
    (fun Loop5(R, I) -> case (I < length(B5)) of true -> R7 = R ++ [-lists:nth((I)+1, B5)], I9 = (I + 1), Loop5(R7, I9); _ -> ok end end(R6, I8)),
    R7.

btAdd(A, B) ->
    btInt((btToInt(A) + btToInt(B5))).

btMul(A, B) ->
    btInt((btToInt(A) * btToInt(B5))).

padLeft(S, W) ->
    R8 = S0,
    (fun Loop6(R) -> case (length(R) < W) of true -> R9 = " " ++ R, Loop6(R9); _ -> ok end end(R8)),
    R9.

show(Label, B) ->
    L = padLeft(Label, 7),
    Bs = padLeft(btToString(B5), 12),
    Is = padLeft(lists:flatten(io_lib:format("~p", [btToInt(B5)])), 7),
    io:format("~p~n", [L ++ " " ++ Bs ++ " " ++ Is]).

main() ->
    Ares = btString("+-0++0+"),
    A = mochi_get("bt", Ares),
    B = btInt(-436),
    Cres = btString("+-++-"),
    C = mochi_get("bt", Cres),
    show("a:", A),
    show("b:", B),
    show("c:", C),
    show("a(b-c):", btMul(A, btAdd(B, btNeg(C)))).

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
