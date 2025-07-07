#!/usr/bin/env escript
%% if_else.erl - manual translation of tests/vm/valid/if_else.mochi

main(_) ->
    X = 5,
    if
        X > 3 -> io:format("big~n");
        true -> io:format("small~n")
    end.
