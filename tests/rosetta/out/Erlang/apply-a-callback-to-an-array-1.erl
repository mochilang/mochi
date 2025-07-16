% apply-a-callback-to-an-array-1.erl - generated from apply-a-callback-to-an-array-1.mochi

main(_) ->
    lists:foreach(fun(I) -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [(I * I)]))]) end, [1, 2, 3, 4, 5]).
