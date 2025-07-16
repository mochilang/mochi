% arithmetic-integer-1.erl - generated from arithmetic-integer-1.mochi

main() ->
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " + " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [(A + B)]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " - " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [(A - B)]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " * " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [(A * B)]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " / " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [((A / B))]))]),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " % " ++ lists:flatten(io_lib:format("~p", [B])) ++ " = " ++ lists:flatten(io_lib:format("~p", [rem(A, B)]))]).

main(_) ->
    main().
