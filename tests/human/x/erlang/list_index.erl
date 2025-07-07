#!/usr/bin/env escript
%% list_index.erl - manual translation of tests/vm/valid/list_index.mochi

main(_) ->
    Xs = [10,20,30],
    io:format("~p~n", [lists:nth(2, Xs)]).
