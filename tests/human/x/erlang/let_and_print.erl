#!/usr/bin/env escript
%% let_and_print.erl - manual translation of tests/vm/valid/let_and_print.mochi

main(_) ->
    A = 10,
    B = 20,
    io:format("~p~n", [A + B]).
