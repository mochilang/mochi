% babbage-problem.erl - generated from babbage-problem.mochi

main(_) ->
    N0 = 1,
    (fun Loop0(N) -> case true of true -> Square = (N * N), Ending = rem(Square, 1000000), (case (Ending == 269696) of true -> io:format("~p~n", ["The smallest number whose square ends with " ++ lists:flatten(io_lib:format("~p", [269696])) ++ " is " ++ lists:flatten(io_lib:format("~p", [N]))]), throw(break); _ -> ok end), N1 = (N + 1), Loop0(N1); _ -> ok end end(N0)).
