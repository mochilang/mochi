#!/usr/bin/env escript
%% len_string.erl - manual translation of tests/vm/valid/len_string.mochi

main(_) ->
    io:format("~p~n", [length("mochi")]).
