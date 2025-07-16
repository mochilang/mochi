% call-a-foreign-language-function.erl - generated from call-a-foreign-language-function.mochi

strdup(S) ->
    S ++ "".

main() ->
    C2 = strdup(Go1),
    io:format("~p~n", [C2]).

main(_) ->
    main().
