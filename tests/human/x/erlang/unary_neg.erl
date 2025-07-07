#!/usr/bin/env escript
%% unary_neg.erl - manual translation of tests/vm/valid/unary_neg.mochi

main(_) ->
    io:format("~p~n", [-3]),
    io:format("~p~n", [5 + (-2)]).
