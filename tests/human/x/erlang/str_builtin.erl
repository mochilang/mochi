#!/usr/bin/env escript
%% str_builtin.erl - manual translation of tests/vm/valid/str_builtin.mochi

main(_) ->
    io:format("~s~n", [integer_to_list(123)]).
