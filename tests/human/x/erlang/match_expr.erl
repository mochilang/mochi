#!/usr/bin/env escript
%% match_expr.erl - manual translation of tests/vm/valid/match_expr.mochi

main(_) ->
    X = 2,
    Label = case X of
        1 -> "one";
        2 -> "two";
        3 -> "three";
        _ -> "unknown"
    end,
    io:format("~s~n", [Label]).
