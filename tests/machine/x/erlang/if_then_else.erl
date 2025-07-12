#!/usr/bin/env escript
% if_then_else.erl - generated from if_then_else.mochi

main(_) ->
    X = 12,
    Msg = (case (12 > 10) of true -> "yes"; _ -> "no" end),
    io:format("~p~n", [Msg]).
