#!/usr/bin/env escript
% if_else.erl - generated from if_else.mochi

main(_) ->
    X = 5,
    (if (X > 3) -> io:format("~p~n", ["big"]); true -> io:format("~p~n", ["small"]) end).
