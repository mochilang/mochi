% array-length.erl - generated from array-length.mochi

main() ->
    Arr = ["apple", "orange", "pear"],
    io:format("~p~n", ["Length of " ++ lists:flatten(io_lib:format("~p", [Arr])) ++ " is " ++ lists:flatten(io_lib:format("~p", [length(Arr)])) ++ "."]).

main(_) ->
    main().
