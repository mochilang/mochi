% bitmap-midpoint-circle-algorithm.erl - generated from bitmap-midpoint-circle-algorithm.mochi

initGrid(Size) ->
    G0 = [],
    Y0 = 0,
    (fun Loop1(Y, G) -> case (Y < Size) of true -> Row0 = [], X0 = 0, (fun Loop0(Row, X) -> case (X < Size) of true -> Row1 = Row ++ [" "], X1 = (X + 1), Loop0(Row1, X1); _ -> ok end end(Row0, X0)), G1 = G ++ [Row1], Y1 = (Y + 1), Loop1(G1, Y1); _ -> ok end end(Y0, G0)),
    G1.

set(G, X, Y) ->
    (case ((((X1 >= 0) andalso (X1 < length(lists:nth((0)+1, G1)))) andalso (Y1 >= 0)) andalso (Y1 < length(G1))) of true -> GInner0 = mochi_get(Y1, G1), GInnerUpd0 = maps:put(X1, "#", GInner0), G2 = G1#{Y1 => GInnerUpd0}; _ -> ok end).

circle(R) ->
    Size = ((R * 2) + 1),
    G3 = initGrid(Size),
    X2 = R,
    Y2 = 0,
    Err0 = (1 - R),
    (fun Loop2(Y) -> case (Y =< X2) of true -> set(G3, (R + X2), (R + Y)), set(G3, (R + Y), (R + X2)), set(G3, (R - X2), (R + Y)), set(G3, (R - Y), (R + X2)), set(G3, (R - X2), (R - Y)), set(G3, (R - Y), (R - X2)), set(G3, (R + X2), (R - Y)), set(G3, (R + Y), (R - X2)), Y3 = (Y + 1), (case (Err0 < 0) of true -> Err1 = ((Err0 + (2 * Y)) + 1); _ -> X3 = (X2 - 1), Err2 = ((Err1 + (2 * ((Y - X3)))) + 1) end), Loop2(Y3); _ -> ok end end(Y2)),
    G3.

trimRight(Row) ->
    End0 = length(Row1),
    (fun Loop3(End) -> case ((End > 0) andalso (lists:nth(((End - 1))+1, Row1) == " ")) of true -> End1 = (End - 1), Loop3(End1); _ -> ok end end(End0)),
    S0 = "",
    I0 = 0,
    (fun Loop4(S, I) -> case (I < End1) of true -> S1 = (S + lists:nth((I)+1, Row1)), I1 = (I + 1), Loop4(S1, I1); _ -> ok end end(S0, I0)),
    S1.

main(_) ->
    G4 = circle(10),
    lists:foreach(fun({Row,_}) -> io:format("~p~n", [trimRight(Row1)]) end, maps:to_list(G4)).

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
