% angle-difference-between-two-bearings-1.erl - generated from angle-difference-between-two-bearings-1.mochi

angleDiff(B1, B2) ->
    D = (B2 - B1),
    (case (D < (0 - 180)) of true -> (D + 360); _ -> ok end),
    (case (D > 180) of true -> (D - 360); _ -> ok end),
    D.

main(_) ->
    TestCases0 = [[20, 45], [(0 - 45), 45], [(0 - 85), 90], [(0 - 95), 90], [(0 - 45), 125], [(0 - 45), 145], [29.4803, (0 - 88.6381)], [(0 - 78.3251), (0 - 159.036)]],
    lists:foreach(fun(Tc) -> io:format("~p~n", [angleDiff(lists:nth((0)+1, Tc), lists:nth((1)+1, Tc))]) end, TestCases0).
