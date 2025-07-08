#!/usr/bin/env escript
% append_builtin.erl - generated from append_builtin.mochi

main(_) ->
    A = [1, 2],
    io:format("~p~n", [A ++ [3]]).
