% character-codes-2.erl - generated from character-codes-2.mochi

ord(Ch) ->
    (case (Ch == "a") of true -> 97; _ -> ok end),
    (case (Ch == "π") of true -> 960; _ -> ok end),
    (case (Ch == "A") of true -> 65; _ -> ok end),
    0.

chr(N) ->
    (case (N == 97) of true -> "a"; _ -> ok end),
    (case (N == 960) of true -> "π"; _ -> ok end),
    (case (N == 65) of true -> "A"; _ -> ok end),
    "?".

main(_) ->
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [ord("A")]))]),
    io:format("~p~n", [chr(65)]).
