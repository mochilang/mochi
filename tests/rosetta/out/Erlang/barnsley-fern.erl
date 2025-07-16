% barnsley-fern.erl - generated from barnsley-fern.mochi

randInt(S, N) ->
    Next = rem((((S * 1664525) + 1013904223)), 2147483647),
    [Next, rem(Next, N)].

main(_) ->
    XMin = -2.182,
    Dx = (2.6558 - XMin),
    Dy = (9.9983 - 0),
    Height = (((60 * Dy) / Dx)),
    Grid0 = [],
    Row0 = 0,
    (fun Loop1(Grid, Row) -> case (Row < Height) of true -> Line0 = [], Col0 = 0, (fun Loop0(Col, Line) -> case (Col < 60) of true -> Line1 = Line ++ [" "], Col1 = (Col + 1), Loop0(Line1, Col1); _ -> ok end end(Col0, Line0)), Grid1 = Grid ++ [Line1], Row1 = (Row + 1), Loop1(Grid1, Row1); _ -> ok end end(Grid0, Row0)),
    Seed0 = 1,
    X0 = 0,
    Y0 = 0,
    Ix0 = ((((60) * ((X0 - XMin))) / Dx)),
    Iy0 = ((((Height) * ((9.9983 - Y0))) / Dy)),
    (case ((((Ix0 >= 0) andalso (Ix0 < 60)) andalso (Iy0 >= 0)) andalso (Iy0 < Height)) of true -> GridInner0 = mochi_get(Iy0, Grid1), GridInnerUpd0 = maps:put(Ix0, "*", GridInner0), Grid2 = Grid1#{Iy0 => GridInnerUpd0}; _ -> ok end),
    I0 = 0,
    (fun Loop2(Seed, Ix, Iy, I) -> case (I < 10000) of true -> Res0 = randInt(Seed, 100), Seed1 = lists:nth((0)+1, Res0), R = lists:nth((1)+1, Res0), (case (R < 85) of true -> Nx = ((0.85 * X0) + (0.04 * Y0)), Ny = (((-0.04 * X0) + (0.85 * Y0)) + 1.6), X1 = Nx, Y1 = Ny; _ -> (case (R < 92) of true -> Nx = ((0.2 * X1) - (0.26 * Y1)), Ny = (((0.23 * X1) + (0.22 * Y1)) + 1.6), X2 = Nx, Y2 = Ny; _ -> (case (R < 99) of true -> Nx = ((-0.15 * X2) + (0.28 * Y2)), Ny = (((0.26 * X2) + (0.24 * Y2)) + 0.44), X3 = Nx, Y3 = Ny; _ -> X4 = 0, Y4 = (0.16 * Y3) end) end) end), Ix1 = ((((60) * ((X4 - XMin))) / Dx)), Iy1 = ((((Height) * ((9.9983 - Y4))) / Dy)), (case ((((Ix >= 0) andalso (Ix < 60)) andalso (Iy >= 0)) andalso (Iy < Height)) of true -> GridInner1 = mochi_get(Iy, Grid2), GridInnerUpd1 = maps:put(Ix, "*", GridInner1), Grid3 = Grid2#{Iy => GridInnerUpd1}; _ -> ok end), I1 = (I + 1), Loop2(Seed1, Ix1, Iy1, I1); _ -> ok end end(Seed0, Ix0, Iy0, I0)),
    Row2 = 0,
    (fun Loop4(Row) -> case (Row < Height) of true -> Line2 = "", Col2 = 0, (fun Loop3(Line, Col) -> case (Col < 60) of true -> Line3 = (Line + mochi_get(Col, mochi_get(Row, Grid3))), Col3 = (Col + 1), Loop3(Line3, Col3); _ -> ok end end(Line2, Col2)), io:format("~p~n", [Line3]), Row3 = (Row + 1), Loop4(Row3); _ -> ok end end(Row2)).

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
