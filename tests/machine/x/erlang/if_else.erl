#!/usr/bin/env escript
% if_else.erl - generated from if_else.mochi

main(_) ->
    X = 5,
    (case (5 > 3) of true -> io:format("~p~n", ["big"]); _ -> io:format("~p~n", ["small"]) end).
