% animation.erl - generated from animation.mochi

main(_) ->
    Shift0 = 0,
    Inc0 = 1,
    Clicks0 = 0,
    Frames0 = 0,
    (fun Loop1(Shift, Frames) -> case (Clicks0 < 5) of true -> Line0 = "", I0 = 0, (fun Loop0(I, Line) -> case (I < length("Hello World! ")) of true -> Idx = rem(((Shift + I)), length("Hello World! ")), Line1 = (Line + lists:sublist("Hello World! ", (Idx)+1, ((Idx + 1))-(Idx))), I1 = (I + 1), Loop0(Line1, I1); _ -> ok end end(I0, Line0)), io:format("~p~n", [Line1]), Shift1 = rem(((Shift + Inc0)), length("Hello World! ")), Frames1 = (Frames + 1), (case (rem(Frames, length("Hello World! ")) == 0) of true -> Inc1 = (length("Hello World! ") - Inc0), Clicks1 = (Clicks0 + 1); _ -> ok end), Loop1(Shift1, Frames1); _ -> ok end end(Shift0, Frames0)).
