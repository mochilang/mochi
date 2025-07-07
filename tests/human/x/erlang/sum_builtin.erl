#!/usr/bin/env escript
%% sum_builtin.erl - manual translation of tests/vm/valid/sum_builtin.mochi

main(_) ->
    io:format("~p~n", [lists:sum([1,2,3])]).
