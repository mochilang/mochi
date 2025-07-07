#!/usr/bin/env escript
%% binary_precedence.erl - manual translation of tests/vm/valid/binary_precedence.mochi

main(_) ->
    io:format("~p~n", [1 + 2 * 3]),
    io:format("~p~n", [(1 + 2) * 3]),
    io:format("~p~n", [2 * 3 + 1]),
    io:format("~p~n", [2 * (3 + 1)]).
