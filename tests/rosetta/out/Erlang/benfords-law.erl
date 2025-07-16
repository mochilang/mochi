% benfords-law.erl - generated from benfords-law.mochi

floorf(X) ->
    (X).

indexOf(S, Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    -1.

fmtF3(X) ->
    Y0 = (floorf(((X * 1000) + 0.5)) / 1000),
    S0 = lists:flatten(io_lib:format("~p", [Y0])),
    Dot0 = indexOf(S0, "."),
    (case (Dot0 == (0 - 1)) of true -> S1 = S0 ++ ".000"; _ -> Decs0 = ((length(S1) - Dot0) - 1), (case (Decs0 > 3) of true -> S2 = string:substr(S1, (0)+1, ((Dot0 + 4))-(0)); _ -> (fun Loop1(Decs, S) -> case (Decs < 3) of true -> S3 = S ++ "0", Decs1 = (Decs + 1), Loop1(S3, Decs1); _ -> ok end end(Decs0, S2)) end) end),
    S3.

padFloat3(X, Width) ->
    S4 = fmtF3(X),
    (fun Loop2(S) -> case (length(S) < Width) of true -> S5 = " " ++ S, Loop2(S5); _ -> ok end end(S4)),
    S5.

fib1000() ->
    A0 = 0,
    B0 = 1,
    Res0 = [],
    I2 = 0,
    (fun Loop3(A, I, Res, B) -> case (I < 1000) of true -> Res1 = Res ++ [B], T0 = B, B1 = (B + A), A1 = T0, I3 = (I + 1), Loop3(Res1, B1, A1, I3); _ -> ok end end(A0, I2, Res0, B0)),
    Res1.

leadingDigit(X) ->
    (case (X < 0) of true -> X0 = -X; _ -> ok end),
    (fun Loop4(X) -> case (X >= 10) of true -> X1 = (X / 10), Loop4(X1); _ -> ok end end(X0)),
    (fun Loop5(X) -> case ((X > 0) andalso (X < 1)) of true -> X2 = (X * 10), Loop5(X2); _ -> ok end end(X1)),
    X2.

show(Nums, Title) ->
    Counts0 = [0, 0, 0, 0, 0, 0, 0, 0, 0],
    lists:foreach(fun(N) -> D = leadingDigit(N), (case ((D >= 1) andalso (D =< 9)) of true -> Counts1 = lists:sublist(Counts0, (D - 1)) ++ [(lists:nth(((D - 1))+1, Counts0) + 1)] ++ lists:nthtail(((D - 1))+1, Counts0); _ -> ok end) end, Nums),
    Preds = [0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046],
    Total = length(Nums),
    io:format("~p~n", [Title]),
    io:format("~p~n", ["Digit  Observed  Predicted"]),
    I4 = 0,
    (fun Loop6(I) -> case (I < 9) of true -> Obs = ((lists:nth((I)+1, Counts1)) / (Total)), Line0 = "  " ++ lists:flatten(io_lib:format("~p", [(I + 1)])) ++ "  " ++ padFloat3(Obs, 9) ++ "  " ++ padFloat3(lists:nth((I)+1, Preds), 8), io:format("~p~n", [Line0]), I5 = (I + 1), Loop6(I5); _ -> ok end end(I4)).

main() ->
    show(fib1000(), "First 1000 Fibonacci numbers").

main(_) ->
    main().
