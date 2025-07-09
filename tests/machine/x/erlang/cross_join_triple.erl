#!/usr/bin/env escript
% cross_join_triple.erl - generated from cross_join_triple.mochi

main(_) ->
    Nums = [1, 2],
    Letters = ["A", "B"],
    Bools = [true, false],
    Combos = [#{n => N, l => L, b => B} || N <- Nums, L <- Letters, B <- Bools],
    io:format("~p~n", ["--- Cross Join of three lists ---"]),
    lists:foreach(fun(C) -> io:format("~p ~p ~p~n", [maps:get(n, C), maps:get(l, C), maps:get(b, C)]) end, Combos).
