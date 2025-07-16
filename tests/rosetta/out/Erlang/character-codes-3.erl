% character-codes-3.erl - generated from character-codes-3.mochi

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
    B0 = ord("a"),
    R0 = ord("π"),
    S0 = "aπ",
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [B0])) ++ " " ++ lists:flatten(io_lib:format("~p", [R0])) ++ " " ++ S0]),
    io:format("~p~n", ["string cast to []rune: [" ++ lists:flatten(io_lib:format("~p", [B0])) ++ " " ++ lists:flatten(io_lib:format("~p", [R0])) ++ "]"]),
    io:format("~p~n", ["    string range loop: " ++ lists:flatten(io_lib:format("~p", [B0])) ++ " " ++ lists:flatten(io_lib:format("~p", [R0]))]),
    io:format("~p~n", ["         string bytes: 0x61 0xcf 0x80"]).
