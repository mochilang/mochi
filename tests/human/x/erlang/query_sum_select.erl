#!/usr/bin/env escript
%% query_sum_select.erl - manual translation of tests/vm/valid/query_sum_select.mochi

main(_) ->
    Nums = [1,2,3],
    Filt = [N || N <- Nums, N > 1],
    Result = lists:sum(Filt),
    io:format("~p~n", [Result]).
