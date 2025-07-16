% angle-difference-between-two-bearings-2.erl - generated from angle-difference-between-two-bearings-2.mochi

angleDiff(B1, B2) ->
    Diff = (B2 - B1),
    ((rem((((rem(Diff, 360) + 360) + 180)), 360)) - 180).

main(_) ->
    TestCases0 = [[20, 45], [(0 - 45), 45], [(0 - 85), 90], [(0 - 95), 90], [(0 - 45), 125], [(0 - 45), 145], [29.4803, (0 - 88.6381)], [(0 - 78.3251), (0 - 159.036)], [(0 - 70099.74233810938), 29840.67437876723], [(0 - 165313.6666297357), 33693.9894517456], [1174.8380510598456, (0 - 154146.66490124757)], [60175.77306795546, 42213.07192354373]],
    lists:foreach(fun(Tc) -> io:format("~p~n", [angleDiff(lists:nth((0)+1, Tc), lists:nth((1)+1, Tc))]) end, TestCases0).
