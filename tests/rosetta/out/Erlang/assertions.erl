% assertions.erl - generated from assertions.mochi

main() ->
    (case (X /= 42) of true -> io:format("~p~n", ["Assertion failed"]); _ -> io:format("~p~n", ["Assertion passed"]) end).

main(_) ->
    main().
