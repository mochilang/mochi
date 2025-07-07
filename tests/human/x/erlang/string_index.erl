#!/usr/bin/env escript
%% string_index.erl - manual translation of tests/vm/valid/string_index.mochi

main(_) ->
    S = "mochi",
    C = lists:nth(2, S),
    io:format("~c~n", [C]).
