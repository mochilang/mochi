#!/usr/bin/env escript
% if_then_else_nested.erl - generated from if_then_else_nested.mochi

main(_) ->
    X = 8,
    Msg = (if (X > 10) -> "big"; true -> (if (X > 5) -> "medium"; true -> "small" end) end),
    io:format("~p~n", [Msg]).
