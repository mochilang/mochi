% anonymous-recursion.erl - generated from anonymous-recursion.mochi

fib(N) ->
    case (N < 2) of true -> N; _ -> (fib((N - 1)) + fib((N - 2))) end.

main() ->
    I0 = -1,
    (fun Loop0(I) -> case (I =< 10) of true -> (case (I < 0) of true -> io:format("~p~n", ["fib(" ++ lists:flatten(io_lib:format("~p", [I])) ++ ") returned error: negative n is forbidden"]); _ -> io:format("~p~n", ["fib(" ++ lists:flatten(io_lib:format("~p", [I])) ++ ") = " ++ lists:flatten(io_lib:format("~p", [fib(I)]))]) end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)).

main(_) ->
    main().
