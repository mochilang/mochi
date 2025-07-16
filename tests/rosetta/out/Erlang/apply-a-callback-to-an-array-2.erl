% apply-a-callback-to-an-array-2.erl - generated from apply-a-callback-to-an-array-2.mochi

each(Xs, F) ->
    lists:foreach(fun(X) -> f(X) end, Xs).

Map(Xs, F) ->
    R0 = [],
    {R1} = lists:foldl(fun(X, {R}) -> R1 = R ++ [f(X)], {R1} end, {R0}, Xs),
    R1.

main() ->
    S = [1, 2, 3, 4, 5],
    each(S, fun(I) -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [(I * I)]))]) end),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Map(S, fun(I) -> (I * I) end)]))]).

main(_) ->
    main().
