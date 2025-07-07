#!/usr/bin/env escript
%% basic_compare.erl - manual translation of tests/vm/valid/basic_compare.mochi

main(_) ->
    A = 10 - 3,
    B = 2 + 2,
    io:format("~p~n", [A]),
    io:format("~p~n", [A =:= 7]),
    io:format("~p~n", [B < 5]).
