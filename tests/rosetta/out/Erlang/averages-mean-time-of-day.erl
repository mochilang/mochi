% averages-mean-time-of-day.erl - generated from averages-mean-time-of-day.mochi

sinApprox(X) ->
    Term0 = X,
    Sum0 = X,
    N0 = 1,
    (fun Loop0(Term, Sum, N) -> case (N =< 8) of true -> Denom = ((((2 * N)) * (((2 * N) + 1)))), Term1 = (((-Term * X) * X) / Denom), Sum1 = (Sum + Term), N1 = (N + 1), Loop0(Term1, Sum1, N1); _ -> ok end end(Term0, Sum0, N0)),
    Sum1.

cosApprox(X) ->
    Term2 = 1,
    Sum2 = 1,
    N2 = 1,
    (fun Loop1(Term, Sum, N) -> case (N =< 8) of true -> Denom = (((((2 * N) - 1)) * ((2 * N)))), Term3 = (((-Term * X) * X) / Denom), Sum3 = (Sum + Term), N3 = (N + 1), Loop1(Term3, Sum3, N3); _ -> ok end end(Term2, Sum2, N2)),
    Sum3.

atanApprox(X) ->
    (case (X > 1) of true -> ((3.141592653589793 / 2) - (X / (((X * X) + 0.28)))); _ -> ok end),
    (case (X < (-1)) of true -> ((-3.141592653589793 / 2) - (X / (((X * X) + 0.28)))); _ -> ok end),
    (X / ((1 + ((0.28 * X) * X)))).

atan2Approx(Y, X) ->
    (case (X > 0) of true -> atanApprox((Y / X)); _ -> ok end),
    (case (X < 0) of true -> (case (Y >= 0) of true -> (atanApprox((Y / X)) + 3.141592653589793); _ -> ok end), (atanApprox((Y / X)) - 3.141592653589793); _ -> ok end),
    (case (Y > 0) of true -> (3.141592653589793 / 2); _ -> ok end),
    (case (Y < 0) of true -> (-3.141592653589793 / 2); _ -> ok end),
    0.

digit(Ch) ->
    I0 = 0,
    (fun Loop2(I) -> case (I < length(Digits)) of true -> (case (string:substr(Digits, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop2(I1); _ -> ok end end(I0)),
    0.

parseTwo(S, Idx) ->
    ((digit(string:substr(S, (Idx)+1, ((Idx + 1))-(Idx))) * 10) + digit(string:substr(S, ((Idx + 1))+1, ((Idx + 2))-((Idx + 1))))).

parseSec(S) ->
    H = parseTwo(S, 0),
    M = parseTwo(S, 3),
    Sec = parseTwo(S, 6),
    ((((((H * 60) + M)) * 60) + Sec)).

pad(N) ->
    case (N3 < 10) of true -> "0" ++ lists:flatten(io_lib:format("~p", [N3])); _ -> lists:flatten(io_lib:format("~p", [N3])) end.

meanTime(Times) ->
    Ssum0 = 0,
    Csum0 = 0,
    I2 = 0,
    (fun Loop3(Ssum, Csum, I) -> case (I < length(Times)) of true -> Sec = parseSec(lists:nth((I)+1, Times)), Ang = (((Sec * 2) * 3.141592653589793) / 86400), Ssum1 = (Ssum + sinApprox(Ang)), Csum1 = (Csum + cosApprox(Ang)), I3 = (I + 1), Loop3(Ssum1, Csum1, I3); _ -> ok end end(Ssum0, Csum0, I2)),
    Theta0 = atan2Approx(Ssum1, Csum1),
    Frac0 = (Theta0 / ((2 * 3.141592653589793))),
    (fun Loop4(Frac) -> case (Frac < 0) of true -> Frac1 = (Frac + 1), Loop4(Frac1); _ -> ok end end(Frac0)),
    Total = (Frac1 * 86400),
    Si = Total,
    H = ((Si / 3600)),
    M = (((rem(Si, 3600)) / 60)),
    S = (rem(Si, 60)),
    pad(H) ++ ":" ++ pad(M) ++ ":" ++ pad(S).

main() ->
    Inputs = ["23:00:17", "23:40:20", "00:12:45", "00:17:19"],
    io:format("~p~n", [meanTime(Inputs)]).

main(_) ->
    main().
