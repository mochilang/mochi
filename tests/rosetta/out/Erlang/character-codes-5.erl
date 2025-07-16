% character-codes-5.erl - generated from character-codes-5.mochi

chr(N) ->
    (case (N == 97) of true -> "a"; _ -> ok end),
    (case (N == 960) of true -> "π"; _ -> ok end),
    (case (N == 65) of true -> "A"; _ -> ok end),
    "?".

main(_) ->
    io:format("~p~n", [chr(97)]),
    io:format("~p~n", [chr(960)]),
    io:format("~p~n", [(chr(97) + chr(960))]).
