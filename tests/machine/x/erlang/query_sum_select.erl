#!/usr/bin/env escript
%% Generated by Mochi compiler v0.10.27 on 2025-07-17T08:58:47Z
% query_sum_select.erl - generated from query_sum_select.mochi

main(_) ->
    Nums = [1, 2, 3],
    Result = lists:sum([N || N <- Nums, (N > 1)]),
    io:format("~p~n", [Result]).
