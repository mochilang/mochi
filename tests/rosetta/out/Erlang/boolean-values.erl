% boolean-values.erl - generated from boolean-values.mochi

parseBool(S) ->
    L = lower(S),
    (case (((((L == "1") orelse (L == "t")) orelse (L == true)) orelse (L == "yes")) orelse (L == "y")) of true -> true; _ -> ok end),
    false.

main() ->
    N0 = true,
    io:format("~p~n", [N0]),
    io:format("~p~n", ["bool"]),
    N1 = not N0,
    io:format("~p~n", [N1]),
    io:format("~p ~p~n", ["x == y:", (X == Y)]),
    io:format("~p ~p~n", ["x < y:", (X < Y)]),
    io:format("~p~n", ["\nConvert String into Boolean Data type\n"]),
    io:format("~p ~p~n", ["Before :", "string"]),
    BolStr = parseBool(Str1),
    io:format("~p ~p~n", ["After :", "bool"]).

main(_) ->
    main().
