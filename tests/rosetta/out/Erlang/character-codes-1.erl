% character-codes-1.erl - generated from character-codes-1.mochi

ord(Ch) ->
    (case (Ch == "a") of true -> 97; _ -> ok end),
    (case (Ch == "π") of true -> 960; _ -> ok end),
    (case (Ch == "A") of true -> 65; _ -> ok end),
    0.

main(_) ->
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [ord("a")]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [ord("π")]))]).
