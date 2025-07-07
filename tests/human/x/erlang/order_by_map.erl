#!/usr/bin/env escript
%% order_by_map.erl - manual translation of tests/vm/valid/order_by_map.mochi

main(_) ->
    Data = [#{a => 1, b => 2},
            #{a => 1, b => 1},
            #{a => 0, b => 5}],
    Sorted = lists:sort(fun(A,B) ->
                {maps:get(a,A), maps:get(b,A)} =< {maps:get(a,B), maps:get(b,B)}
             end, Data),
    io:format("~p~n", [Sorted]).
