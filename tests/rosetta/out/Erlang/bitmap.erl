% bitmap.erl - generated from bitmap.mochi

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
    (fun Loop1(Data, Row) -> case (Row < Y) of true -> R0 = [], Col0 = 0, (fun Loop0(R, Col) -> case (Col < X) of true -> R1 = R ++ [#{"__name" => "Pixel", R => 0, G => 0, B => 0}], Col1 = (Col + 1), Loop0(R1, Col1); _ -> ok end end(R, Col0)), Data1 = Data ++ [R1], Row1 = (Row + 1), Loop1(Row1, Data1); _ -> ok end end(Data0, Row0)),
    #{"__name" => "Bitmap", cols => X, rows => Y, px => Data1}.

Extent(B) ->
    #{"cols" => mochi_get(cols, B), "rows" => mochi_get(rows, B)}.

Fill(B, P) ->
    Y0 = 0,
    (fun Loop3(Y) -> case (Y < mochi_get(rows, B)) of true -> X0 = 0, (fun Loop2(Row, Px, B, X) -> case (X < mochi_get(cols, B)) of true -> Px0 = mochi_get(px, B), Row2 = mochi_get(Y, Px), Row3 = maps:put(X, P, Row), Px1 = maps:put(Y, Row, Px), B0 = B#{px => Px}, X1 = (X + 1), Loop2(Row3, Px1, B0, X1); _ -> ok end end(Row1, Px, B, X0)), Y1 = (Y + 1), Loop3(Y1); _ -> ok end end(Y0)).

FillRgb(B, C) ->
    Fill(B0, pixelFromRgb(C)).

SetPx(B, X, Y, P) ->
    (case ((((X1 < 0) orelse (X1 >= mochi_get(cols, B0))) orelse (Y1 < 0)) orelse (Y1 >= mochi_get(rows, B0))) of true -> false; _ -> ok end),
    Px2 = mochi_get(px, B0),
    Row4 = mochi_get(Y1, Px2),
    Row5 = maps:put(X1, P, Row4),
    Px3 = maps:put(Y1, Row5, Px2),
    B1 = B0#{px => Px3},
    true.

SetPxRgb(B, X, Y, C) ->
    SetPx(B1, X1, Y1, pixelFromRgb(C)).

GetPx(B, X, Y) ->
    (case ((((X1 < 0) orelse (X1 >= mochi_get(cols, B1))) orelse (Y1 < 0)) orelse (Y1 >= mochi_get(rows, B1))) of true -> #{"ok" => false}; _ -> ok end),
    Row = mochi_get(Y1, mochi_get(px, B1)),
    #{"ok" => true, "pixel" => mochi_get(X1, Row)}.

GetPxRgb(B, X, Y) ->
    R = GetPx(B1, X1, Y1),
    (case not mochi_get(ok, R) of true -> #{"ok" => false}; _ -> ok end),
    #{"ok" => true, "rgb" => rgbFromPixel(mochi_get(pixel, R))}.

ppmSize(B) ->
    Header = "P6\n# Creator: Rosetta Code http://rosettacode.org/\n" ++ lists:flatten(io_lib:format("~p", [mochi_get(cols, B1)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(rows, B1)])) ++ "\n255\n",
    (length(Header) + ((3 * mochi_get(cols, B1)) * mochi_get(rows, B1))).

pixelStr(P) ->
    "{" ++ lists:flatten(io_lib:format("~p", [mochi_get(R, P)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(G, P)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(B, P)])) ++ "}".

main() ->
    Bm0 = NewBitmap(300, 240),
    FillRgb(Bm0, 16711680),
    SetPxRgb(Bm0, 10, 20, 255),
    SetPxRgb(Bm0, 20, 30, 0),
    SetPxRgb(Bm0, 30, 40, 1056816),
    C1 = GetPx(Bm0, 0, 0),
    C2 = GetPx(Bm0, 10, 20),
    C3 = GetPx(Bm0, 30, 40),
    io:format("~p~n", ["Image size: " ++ lists:flatten(io_lib:format("~p", [mochi_get(cols, Bm0)])) ++ " × " ++ lists:flatten(io_lib:format("~p", [mochi_get(rows, Bm0)]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [ppmSize(Bm0)])) ++ " bytes when encoded as PPM."]),
    (case mochi_get(ok, C1) of undefined -> ok; false -> ok; _ -> io:format("~p~n", ["Pixel at (0,0) is " ++ pixelStr(mochi_get(pixel, C1))]) end),
    (case mochi_get(ok, C2) of undefined -> ok; false -> ok; _ -> io:format("~p~n", ["Pixel at (10,20) is " ++ pixelStr(mochi_get(pixel, C2))]) end),
    (case mochi_get(ok, C3) of undefined -> ok; false -> ok; _ -> P = mochi_get(pixel, C3), R160 = (mochi_get(R, P) * 257), G160 = (mochi_get(G, P) * 257), B160 = (mochi_get(B, P) * 257), io:format("~p~n", ["Pixel at (30,40) has R=" ++ lists:flatten(io_lib:format("~p", [R160])) ++ ", G=" ++ lists:flatten(io_lib:format("~p", [G160])) ++ ", B=" ++ lists:flatten(io_lib:format("~p", [B160]))]) end).

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
