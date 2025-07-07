#!/usr/bin/env escript
%% if_then_else_nested.erl - manual translation of tests/vm/valid/if_then_else_nested.mochi

main(_) ->
    X = 8,
    Msg = if
        X > 10 -> "big";
        X > 5 -> "medium";
        true -> "small"
    end,
    io:format("~s~n", [Msg]).
