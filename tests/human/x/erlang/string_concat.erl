#!/usr/bin/env escript
%% string_concat.erl - manual translation of tests/vm/valid/string_concat.mochi

main(_) ->
    io:format("~s~n", ["hello " ++ "world"]).
