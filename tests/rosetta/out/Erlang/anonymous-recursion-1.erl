% anonymous-recursion-1.erl - generated from anonymous-recursion-1.mochi

fib(N) ->
    (case (N < 2) of true -> N; _ -> ok end),
    A0 = 0,
    B0 = 1,
    I0 = 1,
    (fun Loop0(A, B, I) -> case (I < N) of true -> T = (A + B), A1 = B, B1 = T, I1 = (I + 1), Loop0(A1, B1, I1); _ -> ok end end(A0, B0, I0)),
    B1.

main() ->
    lists:foreach(fun(N) -> (case (N < 0) of true -> io:format("~p~n", ["fib undefined for negative numbers"]); _ -> io:format("~p~n", ["fib " ++ lists:flatten(io_lib:format("~p", [N])) ++ " = " ++ lists:flatten(io_lib:format("~p", [fib(N)]))]) end) end, [0, 1, 2, 3, 4, 5, 10, 40, -1]).

main(_) ->
    main().
