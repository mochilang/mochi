% bitmap-write-a-ppm-file.erl - generated from bitmap-write-a-ppm-file.mochi

newBitmap(W, H, C) ->
    Rows0 = [],
    Y0 = 0,
    (fun Loop1(Rows, Y) -> case (Y < H) of true -> Row0 = [], X0 = 0, (fun Loop0(Row, X) -> case (X < W) of true -> Row1 = Row ++ [C], X1 = (X + 1), Loop0(Row1, X1); _ -> ok end end(Row0, X0)), Rows1 = Rows ++ [Row1], Y1 = (Y + 1), Loop1(Rows1, Y1); _ -> ok end end(Rows0, Y0)),
    #{"__name" => "Bitmap", width => W, height => H, pixels => Rows1}.

setPixel(B, X, Y, C) ->
    Rows2 = mochi_get(pixels, B),
    Row2 = lists:nth((Y1)+1, Rows2),
    Row3 = lists:sublist(Row2, X1) ++ [C] ++ lists:nthtail((X1)+1, Row2),
    Rows3 = lists:sublist(Rows2, Y1) ++ [Row3] ++ lists:nthtail((Y1)+1, Rows2),
    B0 = B#{pixels => Rows3}.

fillRect(B, X, Y, W, H, C) ->
    Yy0 = Y1,
    (fun Loop3(Yy) -> case (Yy < (Y1 + H)) of true -> Xx0 = X1, (fun Loop2(Xx) -> case (Xx < (X1 + W)) of true -> setPixel(B0, Xx, Yy, C), Xx1 = (Xx + 1), Loop2(Xx1); _ -> ok end end(Xx0)), Yy1 = (Yy + 1), Loop3(Yy1); _ -> ok end end(Yy0)).

pad(N, Width) ->
    S0 = lists:flatten(io_lib:format("~p", [N])),
    (fun Loop4(S) -> case (length(S) < Width) of true -> S1 = " " ++ S, Loop4(S1); _ -> ok end end(S0)),
    S1.

writePPMP3(B) ->
    Maxv0 = 0,
    Y2 = 0,
    (fun Loop6(Y) -> case (Y < mochi_get(height, B0)) of true -> X2 = 0, (fun Loop5(X) -> case (X < mochi_get(width, B0)) of true -> P = mochi_get(X, mochi_get(Y, mochi_get(pixels, B0))), (case (mochi_get(R, P) > Maxv0) of true -> Maxv1 = mochi_get(R, P); _ -> ok end), (case (mochi_get(G, P) > Maxv1) of true -> Maxv2 = mochi_get(G, P); _ -> ok end), (case (mochi_get(B, P) > Maxv2) of true -> Maxv3 = mochi_get(B, P); _ -> ok end), X3 = (X + 1), Loop5(X3); _ -> ok end end(X2)), Y3 = (Y + 1), Loop6(Y3); _ -> ok end end(Y2)),
    Out0 = "P3\n# generated from Bitmap.writeppmp3\n" ++ lists:flatten(io_lib:format("~p", [mochi_get(width, B0)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(height, B0)])) ++ "\n" ++ lists:flatten(io_lib:format("~p", [Maxv3])) ++ "\n",
    Numsize0 = length(lists:flatten(io_lib:format("~p", [Maxv3]))),
    Y4 = (mochi_get(height, B0) - 1),
    (fun Loop8(Out, Y) -> case (Y >= 0) of true -> Line0 = "", X4 = 0, (fun Loop7(Line, X) -> case (X < mochi_get(width, B0)) of true -> P = mochi_get(X, mochi_get(Y, mochi_get(pixels, B0))), Line1 = Line ++ "   " ++ pad(mochi_get(R, P), Numsize0) ++ " " ++ pad(mochi_get(G, P), Numsize0) ++ " " ++ pad(mochi_get(B, P), Numsize0), X5 = (X + 1), Loop7(X5, Line1); _ -> ok end end(Line0, X4)), Out1 = (Out + Line1), (case (Y > 0) of true -> Out2 = Out ++ "\n"; _ -> Out3 = Out ++ "\n" end), Y5 = (Y - 1), Loop8(Out3, Y5); _ -> ok end end(Out0, Y4)),
    Out3.

main() ->
    Black = #{"__name" => "Colour", R => 0, G => 0, B => 0},
    White = #{"__name" => "Colour", R => 255, G => 255, B => 255},
    Bm0 = newBitmap(4, 4, Black),
    fillRect(Bm0, 1, 0, 1, 2, White),
    setPixel(Bm0, 3, 3, #{"__name" => "Colour", R => 127, G => 0, B => 63}),
    Ppm = writePPMP3(Bm0),
    io:format("~p~n", [Ppm]).

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
