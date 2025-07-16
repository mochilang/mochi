% bitmap-ppm-conversion-through-a-pipe.erl - generated from bitmap-ppm-conversion-through-a-pipe.mochi

pixelFromRgb(C) ->
    R = rem((((C / 65536))), 256),
    G = rem((((C / 256))), 256),
    B = rem(C, 256),
    #{"__name" => "Pixel", R => R, G => G, B => B}.

rgbFromPixel(P) ->
    (((mochi_get(R, P) * 65536) + (mochi_get(G, P) * 256)) + mochi_get(B, P)).

NewBitmap(X, Y) ->
    Data0 = [],
    Row0 = 0,
    (fun Loop1(Data, Row) -> case (Row < Y) of true -> R0 = [], Col0 = 0, (fun Loop0(R, Col) -> case (Col < X) of true -> R1 = R ++ [#{"__name" => "Pixel", R => 0, G => 0, B => 0}], Col1 = (Col + 1), Loop0(R1, Col1); _ -> ok end end(R, Col0)), Data1 = Data ++ [R1], Row1 = (Row + 1), Loop1(Data1, Row1); _ -> ok end end(Data0, Row0)),
    #{"__name" => "Bitmap", cols => X, rows => Y, px => Data1}.

FillRgb(B, C) ->
    Y0 = 0,
    P = pixelFromRgb(C),
    (fun Loop3(Y) -> case (Y < mochi_get(rows, B)) of true -> X0 = 0, (fun Loop2(Row, Px, B, X) -> case (X < mochi_get(cols, B)) of true -> Px0 = mochi_get(px, B), Row2 = mochi_get(Y, Px), Row3 = maps:put(X, P, Row), Px1 = maps:put(Y, Row, Px), B0 = B#{px => Px}, X1 = (X + 1), Loop2(B0, X1, Row3, Px1); _ -> ok end end(Row1, Px, B, X0)), Y1 = (Y + 1), Loop3(Y1); _ -> ok end end(Y0)).

SetPxRgb(B, X, Y, C) ->
    (case ((((X1 < 0) orelse (X1 >= mochi_get(cols, B0))) orelse (Y1 < 0)) orelse (Y1 >= mochi_get(rows, B0))) of true -> false; _ -> ok end),
    Px2 = mochi_get(px, B0),
    Row4 = mochi_get(Y1, Px2),
    Row5 = maps:put(X1, pixelFromRgb(C), Row4),
    Px3 = maps:put(Y1, Row5, Px2),
    B1 = B0#{px => Px3},
    true.

nextRand(Seed) ->
    rem((((Seed * 1664525) + 1013904223)), 2147483648).

main() ->
    Bm0 = NewBitmap(400, 300),
    FillRgb(Bm0, 12615744),
    Seed0 = now(),
    I0 = 0,
    (fun Loop4(Seed, I) -> case (I < 2000) of true -> Seed1 = nextRand(Seed), X = rem(Seed, 400), Seed2 = nextRand(Seed), Y = rem(Seed, 300), SetPxRgb(Bm0, X, Y, 8405024), I1 = (I + 1), Loop4(Seed2, I1); _ -> ok end end(Seed0, I0)),
    X2 = 0,
    (fun Loop7(Y, X) -> case (X < 400) of true -> Y2 = 240, (fun Loop5(Y) -> case (Y < 245) of true -> SetPxRgb(Bm0, X, Y, 8405024), Y3 = (Y + 1), Loop5(Y3); _ -> ok end end(Y)), Y4 = 260, (fun Loop6(Y) -> case (Y < 265) of true -> SetPxRgb(Bm0, X, Y, 8405024), Y5 = (Y + 1), Loop6(Y5); _ -> ok end end(Y4)), X3 = (X + 1), Loop7(Y5, X3); _ -> ok end end(Y, X)),
    Y6 = 0,
    (fun Loop10(X, Y) -> case (Y < 300) of true -> X4 = 80, (fun Loop8(X) -> case (X < 85) of true -> SetPxRgb(Bm0, X, Y, 8405024), X5 = (X + 1), Loop8(X5); _ -> ok end end(X)), X6 = 95, (fun Loop9(X) -> case (X < 100) of true -> SetPxRgb(Bm0, X, Y, 8405024), X7 = (X + 1), Loop9(X7); _ -> ok end end(X6)), Y7 = (Y + 1), Loop10(X7, Y7); _ -> ok end end(X3, Y6)).

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
