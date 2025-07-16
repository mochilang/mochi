% aliquot-sequence-classifications.erl - generated from aliquot-sequence-classifications.mochi

indexOf(Xs, Value) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(Xs)) of true -> (case (lists:nth((I)+1, Xs) == Value) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    (0 - 1).

contains(Xs, Value) ->
    (indexOf(Xs, Value) /= (0 - 1)).

maxOf(A, B) ->
    (case (A > B) of true -> A; _ -> B end).

intSqrt(N) ->
    (case (N == 0) of true -> 0; _ -> ok end),
    X0 = N,
    Y0 = (((X0 + 1)) / 2),
    (fun Loop1(X, Y) -> case (Y < X) of true -> X1 = Y, Y1 = (((X + (N / X))) / 2), Loop1(X1, Y1); _ -> ok end end(X0, Y0)),
    X1.

sumProperDivisors(N) ->
    (case (N < 2) of true -> 0; _ -> ok end),
    Sqrt = intSqrt(N),
    Sum0 = 1,
    I2 = 2,
    (fun Loop2(I) -> case (I =< Sqrt) of true -> (case (rem(N, I) == 0) of true -> Sum1 = ((Sum0 + I) + (N / I)); _ -> ok end), I3 = (I + 1), Loop2(I3); _ -> ok end end(I2)),
    (case ((Sqrt * Sqrt) == N) of true -> Sum2 = (Sum1 - Sqrt); _ -> ok end),
    Sum2.

classifySequence(K) ->
    Last0 = K,
    Seq0 = [K],
    (fun Loop3(Last, Seq) -> case true of true -> Last1 = sumProperDivisors(Last), Seq1 = Seq ++ [Last], N = length(Seq), Aliquot0 = "", (case (Last == 0) of true -> Aliquot1 = "Terminating"; _ -> (case ((N == 2) andalso (Last == K)) of true -> Aliquot2 = "Perfect"; _ -> (case ((N == 3) andalso (Last == K)) of true -> Aliquot3 = "Amicable"; _ -> (case ((N >= 4) andalso (Last == K)) of true -> Aliquot4 = "Sociable[" ++ lists:flatten(io_lib:format("~p", [(N - 1)])) ++ "]"; _ -> (case (Last == lists:nth(((N - 2))+1, Seq)) of true -> Aliquot5 = "Aspiring"; _ -> (case contains(lists:sublist(Seq, (1)+1, (maxOf(1, (N - 2)))-(1)), Last) of undefined -> (case ((N == 16) orelse (Last > 140737488355328)) of true -> Aliquot7 = "Non-Terminating"; _ -> ok end); false -> (case ((N == 16) orelse (Last > 140737488355328)) of true -> Aliquot7 = "Non-Terminating"; _ -> ok end); _ -> Idx = indexOf(Seq, Last), Aliquot6 = "Cyclic[" ++ lists:flatten(io_lib:format("~p", [((N - 1) - Idx)])) ++ "]" end) end) end) end) end) end), (case (Aliquot7 /= "") of true -> #{"seq" => Seq, "aliquot" => Aliquot7}; _ -> ok end), Loop3(Last1, Seq1); _ -> ok end end(Last0, Seq0)),
    #{"seq" => Seq1, "aliquot" => ""}.

padLeft(N, W) ->
    S0 = lists:flatten(io_lib:format("~p", [N])),
    (fun Loop4(S) -> case (length(S) < W) of true -> S1 = " " ++ S, Loop4(S1); _ -> ok end end(S0)),
    S1.

padRight(S, W) ->
    R0 = S1,
    (fun Loop5(R) -> case (length(R) < W) of true -> R1 = R ++ " ", Loop5(R1); _ -> ok end end(R0)),
    R1.

joinWithCommas(Seq) ->
    S2 = "[",
    I4 = 0,
    (fun Loop6(S, I) -> case (I < length(Seq1)) of true -> S3 = (S + lists:flatten(io_lib:format("~p", [lists:nth((I)+1, Seq1)]))), (case (I < (length(Seq1) - 1)) of true -> S4 = S ++ ", "; _ -> ok end), I5 = (I + 1), Loop6(S4, I5); _ -> ok end end(S2, I4)),
    S5 = S4 ++ "]",
    S5.

main() ->
    io:format("~p~n", ["Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n"]),
    K0 = 1,
    (fun Loop7(K) -> case (K =< 10) of true -> Res = classifySequence(K), io:format("~p~n", [padLeft(K, 2) ++ ": " ++ padRight(mochi_get("aliquot", Res), 15) ++ " " ++ joinWithCommas(mochi_get("seq", Res))]), K1 = (K + 1), Loop7(K1); _ -> ok end end(K0)),
    io:format("~p~n", [""]),
    S = [11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488],
    I6 = 0,
    (fun Loop8(I) -> case (I < length(S)) of true -> Val = lists:nth((I)+1, S), Res = classifySequence(Val), io:format("~p~n", [padLeft(Val, 7) ++ ": " ++ padRight(mochi_get("aliquot", Res), 15) ++ " " ++ joinWithCommas(mochi_get("seq", Res))]), I7 = (I + 1), Loop8(I7); _ -> ok end end(I6)),
    io:format("~p~n", [""]),
    R = classifySequence(Big),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Big])) ++ ": " ++ padRight(mochi_get("aliquot", R), 15) ++ " " ++ joinWithCommas(mochi_get("seq", R))]).

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
