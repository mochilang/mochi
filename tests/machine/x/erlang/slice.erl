#!/usr/bin/env escript
% slice.erl - generated from slice.mochi

main(_) ->
    io:format("~p~n", [lists:sublist([1, 2, 3], (1)+1, (3)-(1))]),
    io:format("~p~n", [lists:sublist([1, 2, 3], (0)+1, (2)-(0))]),
    io:format("~p~n", [string:substr("hello", (1)+1, (4)-(1))]).
