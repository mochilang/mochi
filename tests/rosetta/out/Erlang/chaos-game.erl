% chaos-game.erl - generated from chaos-game.mochi

randInt(S, N) ->
    Next = rem((((S * 1664525) + 1013904223)), 2147483647),
    [Next, rem(Next, N)].

main(_) ->
    Height = ((60 * 0.86602540378)),
    Grid0 = [],
    Y0 = 0,
    (fun Loop1(Y, Grid) -> case (Y < Height) of true -> Line0 = [], X0 = 0, (fun Loop0(X, Line) -> case (X < 60) of true -> Line1 = Line ++ [" "], X1 = (X + 1), Loop0(Line1, X1); _ -> ok end end(X0, Line0)), Grid1 = Grid ++ [Line1], Y1 = (Y + 1), Loop1(Grid1, Y1); _ -> ok end end(Y0, Grid0)),
    Seed0 = 1,
    Vertices = [[0, (Height - 1)], [(60 - 1), (Height - 1)], [((60 / 2)), 0]],
    Px0 = ((60 / 2)),
    Py0 = ((Height / 2)),
    I0 = 0,
    (fun Loop2(Seed, Px, Py, I) -> case (I < 5000) of true -> R0 = randInt(Seed, 3), Seed1 = lists:nth((0)+1, R0), Idx = lists:nth((1)+1, R0), V = lists:nth((Idx)+1, Vertices), Px1 = ((((Px + lists:nth((0)+1, V))) / 2)), Py1 = ((((Py + lists:nth((1)+1, V))) / 2)), (case ((((Px >= 0) andalso (Px < 60)) andalso (Py >= 0)) andalso (Py < Height)) of true -> GridInner0 = mochi_get(Py, Grid1), GridInnerUpd0 = maps:put(Px, "*", GridInner0), Grid2 = Grid1#{Py => GridInnerUpd0}; _ -> ok end), I1 = (I + 1), Loop2(I1, Seed1, Px1, Py1); _ -> ok end end(Seed0, Px0, Py0, I0)),
    Y2 = 0,
    (fun Loop4(Y) -> case (Y < Height) of true -> Line2 = "", X2 = 0, (fun Loop3(Line, X) -> case (X < 60) of true -> Line3 = (Line + mochi_get(X, mochi_get(Y, Grid2))), X3 = (X + 1), Loop3(Line3, X3); _ -> ok end end(Line2, X2)), io:format("~p~n", [Line3]), Y3 = (Y + 1), Loop4(Y3); _ -> ok end end(Y2)).

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
