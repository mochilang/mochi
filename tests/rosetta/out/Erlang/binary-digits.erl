% binary-digits.erl - generated from binary-digits.mochi

toBin(N) ->
    (case (N == 0) of true -> "0"; _ -> ok end),
    Bits0 = "",
    X0 = N,
    (fun Loop0(Bits, X) -> case (X > 0) of true -> Bits1 = (lists:flatten(io_lib:format("~p", [rem(X, 2)])) + Bits), X1 = ((X / 2)), Loop0(Bits1, X1); _ -> ok end end(Bits0, X0)),
    Bits1.

main(_) ->
    lists:foreach(fun(I) -> io:format("~p~n", [toBin(I)]) end, lists:seq(0, (16)-1)).
