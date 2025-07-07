#!/usr/bin/env escript
%% string_compare.erl - manual translation of tests/vm/valid/string_compare.mochi

main(_) ->
    io:format("~p~n", ["a" < "b"]),
    io:format("~p~n", ["a" =< "a"]),
    io:format("~p~n", ["b" > "a"]),
    io:format("~p~n", ["b" >= "b"]).
