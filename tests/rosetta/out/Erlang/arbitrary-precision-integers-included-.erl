% arbitrary-precision-integers-included-.erl - generated from arbitrary-precision-integers-included-.mochi

pow_int(Base, Exp) ->
    Result0 = 1,
    B0 = Base,
    E0 = Exp,
    (fun Loop0(B, E) -> case (E > 0) of true -> (case (rem(E, 2) == 1) of true -> Result1 = (Result0 * B); _ -> ok end), B1 = (B * B), E1 = ((E / 2)), Loop0(B1, E1); _ -> ok end end(B0, E0)),
    Result1.

pow_big(Base, Exp) ->
    Result2 = 1,
    B2 = Base,
    E2 = Exp,
    (fun Loop1(E, B) -> case (E > 0) of true -> (case (rem(E, 2) == 1) of true -> Result3 = (Result2 * B); _ -> ok end), B3 = (B * B), E3 = ((E / 2)), Loop1(B3, E3); _ -> ok end end(E2, B2)),
    Result3.

main(_) ->
    E10 = pow_int(3, 2),
    E20 = pow_int(4, E10),
    Base0 = 5,
    X0 = pow_big(Base0, E20),
    S0 = lists:flatten(io_lib:format("~p", [X0])),
    io:format("~p ~p ~p ~p ~p ~p~n", ["5^(4^(3^2)) has", length(S0), "digits:", string:substr(S0, (0)+1, (20)-(0)), "...", string:substr(S0, ((length(S0) - 20))+1, (length(S0))-((length(S0) - 20)))]).
