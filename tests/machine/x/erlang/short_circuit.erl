#!/usr/bin/env escript
% short_circuit.erl - generated from short_circuit.mochi

boom(A, B) ->
    io:format("~p~n", ["boom"]),
    true.

main(_) ->
    io:format("~p~n", [(false andalso boom(1, 2))]),
    io:format("~p~n", [(true orelse boom(1, 2))]).
