#!/usr/bin/env escript
% tail_recursion.erl - generated from tail_recursion.mochi

sum_rec(N, Acc) ->
    case (N == 0) of true -> Acc; _ -> sum_rec((N - 1), (Acc + N)) end.

main(_) ->
    io:format("~p~n", [sum_rec(10, 0)]).
