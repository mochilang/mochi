#!/usr/bin/env escript
% list_set_ops.erl - generated from list_set_ops.mochi

main(_) ->
    io:format("~p~n", [ordsets:union([1, 2], [2, 3])]),
    io:format("~p~n", [ordsets:subtract([1, 2, 3], [2])]),
    io:format("~p~n", [ordsets:intersection([1, 2, 3], [2, 4])]),
    io:format("~p~n", [length(([1, 2] ++ [2, 3]))]).
