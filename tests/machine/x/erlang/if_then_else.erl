#!/usr/bin/env escript
% if_then_else.erl - generated from if_then_else.mochi

main(_) ->
    X = 12,
    Msg = (if (X > 10) -> "yes"; true -> "no" end),
    io:format("~p~n", [Msg]).
