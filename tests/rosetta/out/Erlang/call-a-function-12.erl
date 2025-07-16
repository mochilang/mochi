% call-a-function-12.erl - generated from call-a-function-12.mochi

mkAdd(A) ->
    fun(B) -> (A + B) end.

mysum(X, Y) ->
    (X + Y).

partialSum(X) ->
    fun(Y) -> mysum(X, Y) end.

main() ->
    Add2 = mkAdd(2),
    Add3 = mkAdd(3),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Add2(5)])) ++ " " ++ lists:flatten(io_lib:format("~p", [Add3(6)]))]),
    Partial = partialSum(13),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Partial(5)]))]).

main(_) ->
    main().
