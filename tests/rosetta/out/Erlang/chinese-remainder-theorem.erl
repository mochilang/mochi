% chinese-remainder-theorem.erl - generated from chinese-remainder-theorem.mochi

egcd(A, B) ->
    (case (A == 0) of true -> [B, 0, 1]; _ -> ok end),
    Res = egcd(rem(B, A), A),
    G = lists:nth((0)+1, Res),
    X1 = lists:nth((1)+1, Res),
    Y1 = lists:nth((2)+1, Res),
    [G, (Y1 - (((B / A)) * X1)), X1].

modInv(A, M) ->
    R = egcd(A, M),
    (case (lists:nth((0)+1, R) /= 1) of true -> 0; _ -> ok end),
    X = lists:nth((1)+1, R),
    (case (X < 0) of true -> (X + M); _ -> ok end),
    X.

crt(A, N) ->
    Prod0 = 1,
    I0 = 0,
    (fun Loop0(Prod, I) -> case (I < length(N)) of true -> Prod1 = (Prod * lists:nth((I)+1, N)), I1 = (I + 1), Loop0(I1, Prod1); _ -> ok end end(Prod0, I0)),
    X0 = 0,
    I2 = 0,
    (fun Loop1(I, X) -> case (I < length(N)) of true -> Ni = lists:nth((I)+1, N), Ai = lists:nth((I)+1, A), P = (Prod1 / Ni), Inv = modInv(rem(P, Ni), Ni), X1 = (X + ((Ai * Inv) * P)), I3 = (I + 1), Loop1(X1, I3); _ -> ok end end(I2, X)),
    rem(X1, Prod1).

main(_) ->
    N = [3, 5, 7],
    A = [2, 3, 2],
    Res = crt(A, N),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Res])) ++ " <nil>"]).
