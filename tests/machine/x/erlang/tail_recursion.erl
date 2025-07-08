#!/usr/bin/env escript
% tail_recursion.erl - generated from tail_recursion.mochi

sum_rec(N, Acc) ->
    (if (N == 0) -> Acc; true -> ok end),
    sum_rec((N - 1), (Acc + N)).

main(_) ->
    io:format("~p~n", [sum_rec(10, 0)]).
