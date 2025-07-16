% array-concatenation.erl - generated from array-concatenation.mochi

concatInts(A, B) ->
    Out0 = [],
    {Out1} = lists:foldl(fun(V, {Out}) -> Out1 = Out ++ [V], {Out1} end, {Out0}, A),
    {Out2} = lists:foldl(fun(V, {Out}) -> Out2 = Out ++ [V], {Out2} end, {Out1}, B),
    Out2.

concatAny(A, B) ->
    Out3 = [],
    {Out4} = lists:foldl(fun(V, {Out}) -> Out4 = Out ++ [V], {Out4} end, {Out3}, A),
    {Out5} = lists:foldl(fun(V, {Out}) -> Out5 = Out ++ [V], {Out5} end, {Out4}, B),
    Out5.

main(_) ->
    A0 = [1, 2, 3],
    B0 = [7, 12, 60],
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [concatInts(A0, B0)]))]),
    I0 = [1, 2, 3],
    J0 = ["Crosby", "Stills", "Nash", "Young"],
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [concatAny(I0, J0)]))]),
    L0 = [1, 2, 3],
    M0 = [7, 12, 60],
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [concatInts(L0, M0)]))]).
