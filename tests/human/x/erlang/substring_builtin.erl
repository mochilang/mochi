#!/usr/bin/env escript
%% substring_builtin.erl - manual translation of tests/vm/valid/substring_builtin.mochi

main(_) ->
    Str = "mochi",
    Sub = string:substr(Str, 2, 3),
    io:format("~s~n", [Sub]).
