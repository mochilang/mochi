% character-codes-4.erl - generated from character-codes-4.mochi

chr(N) ->
    (case (N == 97) of true -> "a"; _ -> ok end),
    (case (N == 960) of true -> "π"; _ -> ok end),
    (case (N == 65) of true -> "A"; _ -> ok end),
    "?".

main(_) ->
    B0 = 97,
    R0 = 960,
    io:format("~p~n", [chr(97) ++ " " ++ chr(960)]),
    io:format("~p~n", [chr(B0) ++ " " ++ chr(R0)]).
