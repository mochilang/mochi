#!/usr/bin/env escript
%% if_then_else.erl - manual translation of tests/vm/valid/if_then_else.mochi

main(_) ->
    X = 12,
    Msg = if
        X > 10 -> "yes";
        true -> "no"
    end,
    io:format("~s~n", [Msg]).
