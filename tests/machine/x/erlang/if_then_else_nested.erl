#!/usr/bin/env escript
% if_then_else_nested.erl - generated from if_then_else_nested.mochi

main(_) ->
    X = 8,
    Msg = (case (X > 10) of true -> "big"; _ -> (case (X > 5) of true -> "medium"; _ -> "small" end) end),
    io:format("~p~n", [Msg]).
