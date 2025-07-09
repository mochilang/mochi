#!/usr/bin/env escript
% query_sum_select.erl - generated from query_sum_select.mochi

main(_) ->
    Nums = [1, 2, 3],
    Result = [lists:sum(N) || N <- Nums, (N > 1)],
    io:format("~p~n", [Result]).
