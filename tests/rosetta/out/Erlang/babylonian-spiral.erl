% babylonian-spiral.erl - generated from babylonian-spiral.mochi

push(H, It) ->
    H0 = H ++ [It],
    I0 = (length(H0) - 1),
    (fun Loop0(H, I) -> case ((I > 0) andalso (mochi_get("s", lists:nth(((I - 1))+1, H)) > mochi_get("s", lists:nth((I)+1, H)))) of true -> Tmp = lists:nth(((I - 1))+1, H), H1 = lists:sublist(H, (I - 1)) ++ [lists:nth((I)+1, H)] ++ lists:nthtail(((I - 1))+1, H), H2 = lists:sublist(H, I) ++ [Tmp] ++ lists:nthtail((I)+1, H), I1 = (I - 1), Loop0(H2, I1); _ -> ok end end(H0, I0)),
    H2.

step(H, Nv, Dir) ->
    (fun Loop1(Nv, H) -> case ((length(H) == 0) orelse ((Nv * Nv) =< mochi_get("s", lists:nth((0)+1, H)))) of true -> H3 = push(H, #{"s" => (Nv * Nv), "a" => Nv, "b" => 0}), Nv0 = (Nv + 1), Loop1(Nv0, H3); _ -> ok end end(Nv, H2)),
    S = mochi_get("s", lists:nth((0)+1, H3)),
    V0 = [],
    (fun Loop2(H, V) -> case ((length(H) > 0) andalso (mochi_get("s", lists:nth((0)+1, H)) == S)) of true -> It = lists:nth((0)+1, H), H4 = lists:sublist(H, (1)+1, (length(H))-(1)), V1 = V ++ [[mochi_get("a", It), mochi_get("b", It)]], (case (mochi_get("a", It) > mochi_get("b", It)) of true -> H5 = push(H, #{"s" => ((mochi_get("a", It) * mochi_get("a", It)) + (((mochi_get("b", It) + 1)) * ((mochi_get("b", It) + 1)))), "a" => mochi_get("a", It), "b" => (mochi_get("b", It) + 1)}); _ -> ok end), Loop2(H5, V1); _ -> ok end end(H3, V0)),
    List0 = [],
    {List1} = lists:foldl(fun(P, {List}) -> List1 = List ++ [P], {List1} end, {List0}, V1),
    Temp0 = List1,
    lists:foreach(fun(P) -> (case (lists:nth((0)+1, P) /= lists:nth((1)+1, P)) of true -> List2 = List1 ++ [[lists:nth((1)+1, P), lists:nth((0)+1, P)]]; _ -> ok end) end, Temp0),
    Temp1 = List2,
    lists:foreach(fun(P) -> (case (lists:nth((1)+1, P) /= 0) of true -> List3 = List2 ++ [[lists:nth((0)+1, P), -lists:nth((1)+1, P)]]; _ -> ok end) end, Temp1),
    Temp2 = List3,
    lists:foreach(fun(P) -> (case (lists:nth((0)+1, P) /= 0) of true -> List4 = List3 ++ [[-lists:nth((0)+1, P), lists:nth((1)+1, P)]]; _ -> ok end) end, Temp2),
    BestDot0 = -999999999,
    Best0 = Dir,
    lists:foreach(fun(P) -> Cross = ((lists:nth((0)+1, P) * lists:nth((1)+1, Dir)) - (lists:nth((1)+1, P) * lists:nth((0)+1, Dir))), (case (Cross >= 0) of true -> Dot = ((lists:nth((0)+1, P) * lists:nth((0)+1, Dir)) + (lists:nth((1)+1, P) * lists:nth((1)+1, Dir))), (case (Dot > BestDot0) of true -> BestDot1 = Dot, Best1 = P; _ -> ok end); _ -> ok end) end, List4),
    #{"d" => Best1, "heap" => H5, "n" => Nv0}.

positions(N) ->
    Pos0 = [],
    X0 = 0,
    Y0 = 0,
    Dir0 = [0, 1],
    Heap0 = [],
    Nv1 = 1,
    I2 = 0,
    (fun Loop3(Heap, Nv, X, Y, I, Pos, Dir) -> case (I < N) of true -> Pos1 = Pos ++ [[X, Y]], St = step(Heap, Nv, Dir), Dir1 = mochi_get("d", St), Heap1 = mochi_get("heap", St), Nv2 = mochi_get("n", St), X1 = (X + lists:nth((0)+1, Dir)), Y1 = (Y + lists:nth((1)+1, Dir)), I3 = (I + 1), Loop3(Nv2, X1, Y1, I3, Pos1, Dir1, Heap1); _ -> ok end end(Heap0, Nv1, X0, Y0, I2, Pos0, Dir0)),
    Pos1.

pad(S, W) ->
    R0 = S,
    (fun Loop4(R) -> case (length(R) < W) of true -> R1 = R ++ " ", Loop4(R1); _ -> ok end end(R0)),
    R1.

main() ->
    Pts = positions(40),
    io:format("~p~n", ["The first 40 Babylonian spiral points are:"]),
    Line0 = "",
    I4 = 0,
    (fun Loop5(I, Line) -> case (I < length(Pts)) of true -> P = mochi_get(I, Pts), S = pad("(" ++ lists:flatten(io_lib:format("~p", [lists:nth((0)+1, P)])) ++ ", " ++ lists:flatten(io_lib:format("~p", [lists:nth((1)+1, P)])) ++ ")", 10), Line1 = (Line + S), (case (rem(((I + 1)), 10) == 0) of true -> io:format("~p~n", [Line]), Line2 = ""; _ -> ok end), I5 = (I + 1), Loop5(Line2, I5); _ -> ok end end(I4, Line0)).

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
