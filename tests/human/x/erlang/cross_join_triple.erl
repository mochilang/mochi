#!/usr/bin/env escript
%% cross_join_triple.erl - manual translation of tests/vm/valid/cross_join_triple.mochi

main(_) ->
    Nums = [1,2],
    Letters = ["A","B"],
    Bools = [true,false],
    Combos = [#{n => N, l => L, b => B} || N <- Nums, L <- Letters, B <- Bools],
    io:format("--- Cross Join of three lists ---~n"),
    lists:foreach(fun(C) ->
        io:format("~p ~s ~p~n", [maps:get(n,C), maps:get(l,C), maps:get(b,C)])
    end, Combos).
