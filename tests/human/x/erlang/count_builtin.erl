#!/usr/bin/env escript
%% count_builtin.erl - manual translation of tests/vm/valid/count_builtin.mochi

main(_) ->
    io:format("~p~n", [length([1,2,3])]).
