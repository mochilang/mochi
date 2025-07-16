% carmichael-3-strong-pseudoprimes.erl - generated from carmichael-3-strong-pseudoprimes.mochi

mod(N, M) ->
    rem((((rem(N, M)) + M)), M).

isPrime(N) ->
    (case (N < 2) of true -> false; _ -> ok end),
    (case (rem(N, 2) == 0) of true -> (N == 2); _ -> ok end),
    (case (rem(N, 3) == 0) of true -> (N == 3); _ -> ok end),
    D0 = 5,
    (fun Loop0(D) -> case ((D * D) =< N) of true -> (case (rem(N, D) == 0) of true -> false; _ -> ok end), D1 = (D + 2), (case (rem(N, D) == 0) of true -> false; _ -> ok end), D2 = (D + 4), Loop0(D2); _ -> ok end end(D0)),
    true.

pad(N, Width) ->
    S0 = lists:flatten(io_lib:format("~p", [N])),
    (fun Loop1(S) -> case (length(S) < Width) of true -> S1 = " " ++ S, Loop1(S1); _ -> ok end end(S0)),
    S1.

carmichael(P1) ->
    try lists:foreach(fun(H3) -> try try lists:foreach(fun(D) -> try (case ((rem(((((H3 + P1)) * ((P1 - 1)))), D2) == 0) andalso (mod((-P1 * P1), H3) == rem(D2, H3))) of true -> P2 = (1 + (((((P1 - 1)) * ((H3 + P1))) / D2))), (case not isPrime(P2) of true -> throw(continue); _ -> ok end), P3 = (1 + (((P1 * P2) / H3))), (case not isPrime(P3) of true -> throw(continue); _ -> ok end), (case (rem(((P2 * P3)), ((P1 - 1))) /= 1) of true -> throw(continue); _ -> ok end), C = ((P1 * P2) * P3), io:format("~p~n", [pad(P1, 2) ++ "   " ++ pad(P2, 4) ++ "   " ++ pad(P3, 5) ++ "     " ++ lists:flatten(io_lib:format("~p", [C]))]); _ -> ok end) catch throw:continue -> ok end end, lists:seq(1, (((H3 + P1)))-1)) catch throw:break -> ok end catch throw:continue -> ok end end, lists:seq(2, (P1)-1)) catch throw:break -> ok end.

main(_) ->
    io:format("~p~n", ["The following are Carmichael munbers for p1 <= 61:\n"]),
    io:format("~p~n", ["p1     p2      p3     product"]),
    io:format("~p~n", ["==     ==      ==     ======="]),
    lists:foreach(fun(P1) -> (case isPrime(P1) of undefined -> ok; false -> ok; _ -> carmichael(P1) end) end, lists:seq(2, (62)-1)).
