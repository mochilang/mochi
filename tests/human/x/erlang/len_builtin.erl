#!/usr/bin/env escript
%% len_builtin.erl - manual translation of tests/vm/valid/len_builtin.mochi

main(_) ->
    io:format("~p~n", [length([1,2,3])]).
