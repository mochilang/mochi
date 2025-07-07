#!/usr/bin/env escript
%% math_ops.erl - manual translation of tests/vm/valid/math_ops.mochi

main(_) ->
    io:format("~p~n", [6 * 7]),
    io:format("~p~n", [7 / 2]),
    io:format("~p~n", [7 rem 2]).
