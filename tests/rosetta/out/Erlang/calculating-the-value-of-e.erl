% calculating-the-value-of-e.erl - generated from calculating-the-value-of-e.mochi

absf(X) ->
    case (X < 0) of true -> -X; _ -> X end.

pow10(N) ->
    R0 = 1,
    I0 = 0,
    (fun Loop0(R, I) -> case (I < N) of true -> R1 = (R * 10), I1 = (I + 1), Loop0(R1, I1); _ -> ok end end(R0, I0)),
    R1.

formatFloat(F, Prec) ->
    Scale = pow10(Prec),
    Scaled = (((F * Scale)) + 0.5),
    N0 = (Scaled),
    Digits0 = lists:flatten(io_lib:format("~p", [N0])),
    (fun Loop1(Digits) -> case (length(Digits) =< Prec) of true -> Digits1 = "0" ++ Digits, Loop1(Digits1); _ -> ok end end(Digits0)),
    IntPart = string:substr(Digits1, (0)+1, ((length(Digits1) - Prec))-(0)),
    FracPart = string:substr(Digits1, ((length(Digits1) - Prec))+1, (length(Digits1))-((length(Digits1) - Prec))),
    IntPart ++ "." ++ FracPart.

main(_) ->
    Factval0 = 1,
    E0 = 2,
    N1 = 2,
    Term0 = 1,
    (fun Loop2(Factval, N, Term, E) -> case true of true -> Factval1 = (Factval * N), N2 = (N + 1), Term1 = (1 / (Factval)), E1 = (E + Term), (case (absf(Term) < 1e-15) of true -> throw(break); _ -> ok end), Loop2(Factval1, N2, Term1, E1); _ -> ok end end(Factval0, N1, Term0, E0)),
    io:format("~p~n", ["e = " ++ formatFloat(E1, 15)]).
