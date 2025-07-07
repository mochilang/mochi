#!/usr/bin/env escript
%% append_builtin.erl - manual translation of tests/vm/valid/append_builtin.mochi

main(_) ->
    A = [1, 2],
    B = A ++ [3],
    io:format("~p~n", [B]).
