#!/usr/bin/env escript
%% tail_recursion.erl - manual translation of tests/vm/valid/tail_recursion.mochi

sum_rec(0, Acc) -> Acc;
sum_rec(N, Acc) -> sum_rec(N - 1, Acc + N).

main(_) ->
    io:format("~p~n", [sum_rec(10, 0)]).
