#!/usr/bin/env escript
%% in_operator.erl - manual translation of tests/vm/valid/in_operator.mochi

main(_) ->
    Xs = [1,2,3],
    io:format("~p~n", [lists:member(2, Xs)]),
    io:format("~p~n", [not lists:member(5, Xs)]).
