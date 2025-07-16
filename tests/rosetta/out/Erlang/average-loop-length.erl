% average-loop-length.erl - generated from average-loop-length.mochi

absf(X) ->
    case (X < 0) of true -> -X; _ -> X end.

floorf(X) ->
    (X).

indexOf(S, Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    -1.

fmtF(X) ->
    Y0 = (floorf(((X * 10000) + 0.5)) / 10000),
    S0 = lists:flatten(io_lib:format("~p", [Y0])),
    Dot0 = indexOf(S0, "."),
    (case (Dot0 == (0 - 1)) of true -> S1 = S0 ++ ".0000"; _ -> Decs0 = ((length(S1) - Dot0) - 1), (case (Decs0 > 4) of true -> S2 = string:substr(S1, (0)+1, ((Dot0 + 5))-(0)); _ -> (fun Loop1(Decs, S) -> case (Decs < 4) of true -> S3 = S ++ "0", Decs1 = (Decs + 1), Loop1(S3, Decs1); _ -> ok end end(Decs0, S2)) end) end),
    S3.

padInt(N, Width) ->
    S4 = lists:flatten(io_lib:format("~p", [N])),
    (fun Loop2(S) -> case (length(S) < Width) of true -> S5 = " " ++ S, Loop2(S5); _ -> ok end end(S4)),
    S5.

padFloat(X, Width) ->
    S6 = fmtF(X),
    (fun Loop3(S) -> case (length(S) < Width) of true -> S7 = " " ++ S, Loop3(S7); _ -> ok end end(S6)),
    S7.

avgLen(N) ->
    Sum0 = 0,
    Seed0 = 1,
    T0 = 0,
    (fun Loop6(T) -> case (T < Tests) of true -> Visited0 = [], I2 = 0, (fun Loop4(Visited, I) -> case (I < N) of true -> Visited1 = Visited ++ [false], I3 = (I + 1), Loop4(I3, Visited1); _ -> ok end end(Visited0, I2)), X0 = 0, (fun Loop5(Visited, Sum, Seed, X) -> case not lists:nth((X)+1, Visited) of true -> Visited2 = lists:sublist(Visited, X) ++ [true] ++ lists:nthtail((X)+1, Visited), Sum1 = (Sum + 1), Seed1 = rem((((Seed * 1664525) + 1013904223)), 2147483647), X1 = rem(Seed, N), Loop5(Visited2, Sum1, Seed1, X1); _ -> ok end end(Visited1, Sum0, Seed0, X0)), T1 = (T + 1), Loop6(T1); _ -> ok end end(T0)),
    ((Sum1) / Tests).

ana(N) ->
    Nn0 = N,
    Term0 = 1,
    Sum2 = 1,
    I4 = (Nn0 - 1),
    (fun Loop7(Term, Sum, I) -> case (I >= 1) of true -> Term1 = (Term * ((I / Nn0))), Sum3 = (Sum + Term), I5 = (I - 1), Loop7(Term1, Sum3, I5); _ -> ok end end(Term0, Sum2, I4)),
    Sum3.

main() ->
    io:format("~p~n", [" N    average    analytical    (error)"]),
    io:format("~p~n", ["===  =========  ============  ========="]),
    N0 = 1,
    (fun Loop8(N) -> case (N =< Nmax) of true -> A = avgLen(N), B = ana(N), Err = ((absf((A - B)) / B) * 100), Line0 = padInt(N, 3) ++ "  " ++ padFloat(A, 9) ++ "  " ++ padFloat(B, 12) ++ "  (" ++ padFloat(Err, 6) ++ "%)", io:format("~p~n", [Line0]), N1 = (N + 1), Loop8(N1); _ -> ok end end(N0)).

main(_) ->
    main().
