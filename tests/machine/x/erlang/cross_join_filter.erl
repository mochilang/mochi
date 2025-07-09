#!/usr/bin/env escript
% cross_join_filter.erl - generated from cross_join_filter.mochi

main(_) ->
    Nums = [1, 2, 3],
    Letters = ["A", "B"],
    Pairs = [#{n => N, l => L} || N <- Nums, L <- Letters, ((N rem 2) == 0)],
    io:format("~p~n", ["--- Even pairs ---"]),
    lists:foreach(fun(P) -> io:format("~p ~p~n", [maps:get(n, P), maps:get(l, P)]) end, Pairs).
