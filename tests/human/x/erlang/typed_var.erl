#!/usr/bin/env escript
%% typed_var.erl - manual translation of tests/vm/valid/typed_var.mochi

main(_) ->
    X = undefined,
    io:format("~p~n", [X]).
