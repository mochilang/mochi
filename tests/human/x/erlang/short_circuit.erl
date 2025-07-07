#!/usr/bin/env escript
%% short_circuit.erl - manual translation of tests/vm/valid/short_circuit.mochi

main(_) ->
    io:format("~p~n", [false andalso boom(1,2)]),
    io:format("~p~n", [true orelse boom(1,2)]).

boom(_A, _B) ->
    io:format("boom~n"),
    true.
