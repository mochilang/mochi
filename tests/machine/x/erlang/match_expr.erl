#!/usr/bin/env escript
% match_expr.erl - generated from match_expr.mochi

main(_) ->
    X = 2,
    Label = (case 2 of 1 -> "one"; 2 -> "two"; 3 -> "three"; _ -> "unknown" end),
    io:format("~p~n", [Label]).
