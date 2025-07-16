% binary-strings.erl - generated from binary-strings.mochi

char(N) ->
    Idx = (N - 97),
    (case ((Idx < 0) orelse (Idx >= length(Letters))) of true -> "?"; _ -> ok end),
    string:substr(Letters, (Idx)+1, ((Idx + 1))-(Idx)).

fromBytes(Bs) ->
    S0 = "",
    I0 = 0,
    (fun Loop0(S, I) -> case (I < length(Bs)) of true -> S1 = (S + char(lists:nth((I)+1, Bs))), I1 = (I + 1), Loop0(S1, I1); _ -> ok end end(S0, I0)),
    S1.

main(_) ->
    B0 = [98, 105, 110, 97, 114, 121],
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [B0]))]),
    C0 = B0,
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [C0]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [(B0 == C0)]))]),
    D0 = [],
    I2 = 0,
    (fun Loop1(D, I) -> case (I < length(B0)) of true -> D1 = D ++ [lists:nth((I)+1, B0)], I3 = (I + 1), Loop1(I3, D1); _ -> ok end end(D0, I2)),
    D2 = lists:sublist(D1, 1) ++ [97] ++ lists:nthtail((1)+1, D1),
    D3 = lists:sublist(D2, 4) ++ [110] ++ lists:nthtail((4)+1, D2),
    io:format("~p~n", [fromBytes(B0)]),
    io:format("~p~n", [fromBytes(D3)]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [(length(B0) == 0)]))]),
    Z0 = B0 ++ [122],
    io:format("~p~n", [fromBytes(Z0)]),
    Sub0 = lists:sublist(B0, (1)+1, (3)-(1)),
    io:format("~p~n", [fromBytes(Sub0)]),
    F0 = [],
    I4 = 0,
    (fun Loop2(I) -> case (I < length(D3)) of true -> Val = lists:nth((I)+1, D3), (case (Val == 110) of true -> F1 = F0 ++ [109]; _ -> F2 = F1 ++ [Val] end), I5 = (I + 1), Loop2(I5); _ -> ok end end(I4)),
    io:format("~p~n", [fromBytes(D3) ++ " -> " ++ fromBytes(F2)]),
    Rem0 = [],
    Rem1 = Rem0 ++ [lists:nth((0)+1, B0)],
    I6 = 3,
    (fun Loop3(Rem, I) -> case (I < length(B0)) of true -> Rem2 = Rem ++ [lists:nth((I)+1, B0)], I7 = (I + 1), Loop3(I7, Rem2); _ -> ok end end(Rem1, I6)),
    io:format("~p~n", [fromBytes(Rem2)]).
