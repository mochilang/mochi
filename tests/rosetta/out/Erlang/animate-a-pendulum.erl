% animate-a-pendulum.erl - generated from animate-a-pendulum.mochi

sinApprox(X) ->
    Term0 = X,
    Sum0 = X,
    N0 = 1,
    (fun Loop0(Term, Sum, N) -> case (N =< 10) of true -> Denom = ((((2 * N)) * (((2 * N) + 1)))), Term1 = (((-Term * X) * X) / Denom), Sum1 = (Sum + Term), N1 = (N + 1), Loop0(Term1, Sum1, N1); _ -> ok end end(Term0, Sum0, N0)),
    Sum1.

cosApprox(X) ->
    Term2 = 1,
    Sum2 = 1,
    N2 = 1,
    (fun Loop1(Term, Sum, N) -> case (N =< 10) of true -> Denom = (((((2 * N) - 1)) * ((2 * N)))), Term3 = (((-Term * X) * X) / Denom), Sum3 = (Sum + Term), N3 = (N + 1), Loop1(Term3, Sum3, N3); _ -> ok end end(Term2, Sum2, N2)),
    Sum3.

sqrtApprox(X) ->
    Guess0 = X,
    I0 = 0,
    (fun Loop2(Guess, I) -> case (I < 10) of true -> Guess1 = (((Guess + (X / Guess))) / 2), I1 = (I + 1), Loop2(Guess1, I1); _ -> ok end end(Guess0, I0)),
    Guess1.

main(_) ->
    Phi0 = (3.141592653589793 / 4),
    Omega = sqrtApprox((9.81 / 10)),
    T0 = 0,
    {T1} = lists:foldl(fun(Step, {T}) -> Phi = (Phi0 * cosApprox((Omega * T))), Pos = (((10 * sinApprox(Phi)) + 0.5)), io:format("~p~n", [lists:flatten(io_lib:format("~p", [Pos]))]), T1 = (T + 0.2), {T1} end, {T0}, lists:seq(0, (10)-1)).
