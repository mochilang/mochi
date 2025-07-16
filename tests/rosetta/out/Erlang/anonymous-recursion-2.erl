% anonymous-recursion-2.erl - generated from anonymous-recursion-2.mochi

fib(N) ->
    (case (N < 2) of true -> N; _ -> ok end),
    A0 = 0,
    B0 = 1,
    I0 = 1,
    (fun Loop0(A, B, I) -> case (I < N) of true -> T = (A + B), A1 = B, B1 = T, I1 = (I + 1), Loop0(A1, B1, I1); _ -> ok end end(A0, B0, I0)),
    B1.

main() ->
    lists:foreach(fun(I) -> (case (I1 < 0) of true -> io:format("~p~n", ["fib(" ++ lists:flatten(io_lib:format("~p", [I1])) ++ ") returned error: negative n is forbidden"]); _ -> io:format("~p~n", ["fib(" ++ lists:flatten(io_lib:format("~p", [I1])) ++ ") = " ++ lists:flatten(io_lib:format("~p", [fib(I1)]))]) end) end, [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).

main(_) ->
    main().
