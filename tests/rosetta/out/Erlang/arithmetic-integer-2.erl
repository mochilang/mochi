% arithmetic-integer-2.erl - generated from arithmetic-integer-2.mochi

main() ->
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " + " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [(A + B)]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " - " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [(A - B)]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " * " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [(A * B)]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " quo " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [((A / B))]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " rem " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [rem(A, B)]))]).

main(_) ->
    main().
