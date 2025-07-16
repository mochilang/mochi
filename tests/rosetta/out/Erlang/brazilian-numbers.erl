% brazilian-numbers.erl - generated from brazilian-numbers.mochi

sameDigits(N, B) ->
    F0 = rem(N, B),
    N0 = ((N / B)),
    (fun Loop0(N) -> case (N > 0) of true -> (case (rem(N, B) /= F0) of true -> false; _ -> ok end), N1 = ((N / B)), Loop0(N1); _ -> ok end end(N0)),
    true.

isBrazilian(N) ->
    (case (N1 < 7) of true -> false; _ -> ok end),
    (case ((rem(N1, 2) == 0) andalso (N1 >= 8)) of true -> true; _ -> ok end),
    B0 = 2,
    (fun Loop1(B) -> case (B < (N1 - 1)) of true -> (case sameDigits(N1, B) of undefined -> ok; false -> ok; _ -> true end), B1 = (B + 1), Loop1(B1); _ -> ok end end(B0)),
    false.

isPrime(N) ->
    (case (N1 < 2) of true -> false; _ -> ok end),
    (case (rem(N1, 2) == 0) of true -> (N1 == 2); _ -> ok end),
    (case (rem(N1, 3) == 0) of true -> (N1 == 3); _ -> ok end),
    D0 = 5,
    (fun Loop2(D) -> case ((D * D) =< N1) of true -> (case (rem(N1, D) == 0) of true -> false; _ -> ok end), D1 = (D + 2), (case (rem(N1, D) == 0) of true -> false; _ -> ok end), D2 = (D + 4), Loop2(D2); _ -> ok end end(D0)),
    true.

main() ->
    Kinds0 = [" ", " odd ", " prime "],
    try lists:foreach(fun(Kind) -> try io:format("~p~n", ["First 20" ++ Kind ++ "Brazilian numbers:"]), C0 = 0, N2 = 7, (fun Loop4() -> case true of true -> (case isBrazilian(N2) of undefined -> ok; false -> ok; _ -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [N2])) ++ " "]), C1 = (C0 + 1), (case (C1 == 20) of true -> io:format("~p~n", ["\n"]), throw(break); _ -> ok end) end), (case (Kind == " ") of true -> N3 = (N2 + 1); _ -> (case (Kind == " odd ") of true -> N4 = (N3 + 2); _ -> (fun Loop3(N) -> case true of true -> N5 = (N + 2), (case isPrime(N) of undefined -> ok; false -> ok; _ -> throw(break) end), Loop3(N5); _ -> ok end end(N4)) end) end), Loop4(); _ -> ok end end()) catch throw:continue -> ok end end, Kinds0) catch throw:break -> ok end,
    N6 = 7,
    C2 = 0,
    (fun Loop5(N) -> case (C2 < 100000) of true -> (case isBrazilian(N) of undefined -> ok; false -> ok; _ -> C3 = (C2 + 1) end), N7 = (N + 1), Loop5(N7); _ -> ok end end(N6)),
    io:format("~p~n", ["The 100,000th Brazilian number: " ++ lists:flatten(io_lib:format("~p", [(N7 - 1)]))]).

main(_) ->
    main().
