% catalan-numbers-pascals-triangle.erl - generated from catalan-numbers-pascals-triangle.mochi

main(_) ->
    T0 = [],
    {T1} = lists:foldl(fun(_, {T}) -> T1 = T ++ [0], {T1} end, {T0}, lists:seq(0, (((15 + 2)))-1)),
    T2 = lists:sublist(T1, 1) ++ [1] ++ lists:nthtail((1)+1, T1),
    {J3, T5} = lists:foldl(fun(I, {T, J}) -> J0 = I, (fun Loop0(T, J) -> case (J > 1) of true -> T3 = lists:sublist(T, J) ++ [(lists:nth((J)+1, T) + lists:nth(((J - 1))+1, T))] ++ lists:nthtail((J)+1, T), J1 = (J - 1), Loop0(T3, J1); _ -> ok end end(T, J)), T4 = lists:sublist(T3, (I + 1)) ++ [lists:nth((I)+1, T3)] ++ lists:nthtail(((I + 1))+1, T3), J2 = (I + 1), (fun Loop1(T, J) -> case (J > 1) of true -> T5 = lists:sublist(T, J) ++ [(lists:nth((J)+1, T) + lists:nth(((J - 1))+1, T))] ++ lists:nthtail((J)+1, T), J3 = (J - 1), Loop1(T5, J3); _ -> ok end end(T4, J2)), Cat = (lists:nth(((I + 1))+1, T5) - lists:nth((I)+1, T5)), (case (I < 10) of true -> io:format("~p~n", [" " ++ lists:flatten(io_lib:format("~p", [I])) ++ " : " ++ lists:flatten(io_lib:format("~p", [Cat]))]); _ -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [I])) ++ " : " ++ lists:flatten(io_lib:format("~p", [Cat]))]) end), {J3, T5} end, {T2, J}, lists:seq(1, (((15 + 1)))-1)).
