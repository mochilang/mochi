% bitmap-read-a-ppm-file.erl - generated from bitmap-read-a-ppm-file.mochi

newBitmap(W, H, Max) ->
    Rows0 = [],
    Y0 = 0,
    (fun Loop1(Rows, Y) -> case (Y < H) of true -> Row0 = [], X0 = 0, (fun Loop0(X, Row) -> case (X < W) of true -> Row1 = Row ++ [#{"__name" => "Pixel", R => 0, G => 0, B => 0}], X1 = (X + 1), Loop0(Row1, X1); _ -> ok end end(X0, Row0)), Rows1 = Rows ++ [Row1], Y1 = (Y + 1), Loop1(Rows1, Y1); _ -> ok end end(Rows0, Y0)),
    #{"__name" => "Bitmap", w => W, h => H, max => Max, data => Rows1}.

setPx(B, X, Y, P) ->
    Rows2 = mochi_get(data, B),
    Row2 = lists:nth((Y1)+1, Rows2),
    Row3 = lists:sublist(Row2, X1) ++ [P] ++ lists:nthtail((X1)+1, Row2),
    Rows3 = lists:sublist(Rows2, Y1) ++ [Row3] ++ lists:nthtail((Y1)+1, Rows2),
    B0 = B#{data => Rows3}.

getPx(B, X, Y) ->
    mochi_get(X1, mochi_get(Y1, mochi_get(data, B0))).

splitLines(S) ->
    Out0 = [],
    Cur0 = "",
    I0 = 0,
    (fun Loop2(I) -> case (I < length(S)) of true -> Ch = substr(S, I, (I + 1)), (case (Ch == "\n") of true -> Out1 = Out0 ++ [Cur0], Cur1 = ""; _ -> Cur2 = (Cur1 + Ch) end), I1 = (I + 1), Loop2(I1); _ -> ok end end(I0)),
    Out2 = Out1 ++ [Cur2],
    Out2.

splitWS(S) ->
    Out3 = [],
    Cur3 = "",
    I2 = 0,
    (fun Loop3(I) -> case (I < length(S)) of true -> Ch = substr(S, I, (I + 1)), (case ((((Ch == " ") orelse (Ch == "\t")) orelse (Ch == "\r")) orelse (Ch == "\n")) of true -> (case (length(Cur3) > 0) of true -> Out4 = Out3 ++ [Cur3], Cur4 = ""; _ -> ok end); _ -> Cur5 = (Cur4 + Ch) end), I3 = (I + 1), Loop3(I3); _ -> ok end end(I2)),
    (case (length(Cur5) > 0) of true -> Out5 = Out4 ++ [Cur5]; _ -> ok end),
    Out5.

parseIntStr(Str) ->
    I4 = 0,
    Neg0 = false,
    (case ((length(Str) > 0) andalso (string:substr(Str, (0)+1, (1)-(0)) == "-")) of true -> Neg1 = true, I5 = 1; _ -> ok end),
    N0 = 0,
    Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
    (fun Loop4(N, I) -> case (I < length(Str)) of true -> N1 = ((N * 10) + mochi_get(string:substr(Str, (I)+1, ((I + 1))-(I)), Digits)), I6 = (I + 1), Loop4(N1, I6); _ -> ok end end(N0, I5)),
    (case Neg1 of undefined -> ok; false -> ok; _ -> N2 = -N1 end),
    N2.

tokenize(S) ->
    Lines = splitLines(S),
    Toks0 = [],
    I7 = 0,
    (fun Loop6(I) -> case (I < length(Lines)) of true -> Line = mochi_get(I, Lines), (case ((length(Line) > 0) andalso (substr(Line, 0, 1) == "#")) of true -> I8 = (I + 1), throw(continue); _ -> ok end), Parts = splitWS(Line), J0 = 0, (fun Loop5(Toks, J) -> case (J < length(Parts)) of true -> Toks1 = Toks ++ [mochi_get(J, Parts)], J1 = (J + 1), Loop5(Toks1, J1); _ -> ok end end(Toks0, J0)), I9 = (I + 1), Loop6(I9); _ -> ok end end(I7)),
    Toks1.

readP3(Text) ->
    Toks = tokenize(Text),
    (case (length(Toks) < 4) of true -> newBitmap(0, 0, 0); _ -> ok end),
    (case (lists:nth((0)+1, Toks) /= "P3") of true -> newBitmap(0, 0, 0); _ -> ok end),
    W = parseIntStr(lists:nth((1)+1, Toks)),
    H = parseIntStr(lists:nth((2)+1, Toks)),
    Maxv = parseIntStr(lists:nth((3)+1, Toks)),
    Idx0 = 4,
    Bm0 = newBitmap(W, H, Maxv),
    Y2 = (H - 1),
    (fun Loop8(Y) -> case (Y >= 0) of true -> X2 = 0, (fun Loop7(Idx, X) -> case (X < W) of true -> R = parseIntStr(lists:nth((Idx)+1, Toks)), G = parseIntStr(lists:nth(((Idx + 1))+1, Toks)), B = parseIntStr(lists:nth(((Idx + 2))+1, Toks)), setPx(Bm0, X, Y, #{"__name" => "Pixel", R => R, G => G, B => B}), Idx1 = (Idx + 3), X3 = (X + 1), Loop7(Idx1, X3); _ -> ok end end(Idx0, X2)), Y3 = (Y - 1), Loop8(Y3); _ -> ok end end(Y2)),
    Bm0.

toGrey(B) ->
    H = mochi_get(h, B),
    W = mochi_get(w, B),
    M0 = 0,
    Y4 = 0,
    (fun Loop10(Y) -> case (Y < H) of true -> X4 = 0, (fun Loop9(X) -> case (X < W) of true -> P = getPx(B, X, Y), L0 = (((((mochi_get(R, P) * 2126) + (mochi_get(G, P) * 7152)) + (mochi_get(B, P) * 722))) / 10000), (case (L0 > mochi_get(max, B)) of true -> L1 = mochi_get(max, B); _ -> ok end), setPx(B, X, Y, #{"__name" => "Pixel", R => L1, G => L1, B => L1}), (case (L1 > M0) of true -> M1 = L1; _ -> ok end), X5 = (X + 1), Loop9(X5); _ -> ok end end(X4)), Y5 = (Y + 1), Loop10(Y5); _ -> ok end end(Y4)),
    B1 = B#{max => M1}.

pad(N, W) ->
    S0 = lists:flatten(io_lib:format("~p", [N2])),
    (fun Loop11(S) -> case (length(S) < W) of true -> S1 = " " ++ S, Loop11(S1); _ -> ok end end(S0)),
    S1.

writeP3(B) ->
    H = mochi_get(h, B),
    W = mochi_get(w, B),
    Max0 = mochi_get(max, B),
    Digits = length(lists:flatten(io_lib:format("~p", [Max0]))),
    Out6 = "P3\n# generated from Bitmap.writeppmp3\n" ++ lists:flatten(io_lib:format("~p", [W])) ++ " " ++ lists:flatten(io_lib:format("~p", [H])) ++ "\n" ++ lists:flatten(io_lib:format("~p", [Max0])) ++ "\n",
    Y6 = (H - 1),
    (fun Loop13(Out, Y) -> case (Y >= 0) of true -> Line0 = "", X6 = 0, (fun Loop12(Line, X) -> case (X < W) of true -> P = getPx(B, X, Y), Line1 = Line ++ "   " ++ pad(mochi_get(R, P), Digits) ++ " " ++ pad(mochi_get(G, P), Digits) ++ " " ++ pad(mochi_get(B, P), Digits), X7 = (X + 1), Loop12(Line1, X7); _ -> ok end end(Line, X6)), Out7 = (Out + Line1) ++ "\n", Y7 = (Y - 1), Loop13(Out7, Y7); _ -> ok end end(Out6, Y6)),
    Out7.

main(_) ->
    Ppmtxt0 = "P3\n" ++ "# feep.ppm\n" ++ "4 4\n" ++ "15\n" ++ " 0  0  0    0  0  0    0  0  0   15  0 15\n" ++ " 0  0  0    0 15  7    0  0  0    0  0  0\n" ++ " 0  0  0    0  0  0    0 15  7    0  0  0\n" ++ "15  0 15    0  0  0    0  0  0    0  0  0\n",
    io:format("~p~n", ["Original Colour PPM file"]),
    io:format("~p~n", [Ppmtxt0]),
    Bm1 = readP3(Ppmtxt0),
    io:format("~p~n", ["Grey PPM:"]),
    toGrey(Bm1),
    Out = writeP3(Bm1),
    io:format("~p~n", [Out]).

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
