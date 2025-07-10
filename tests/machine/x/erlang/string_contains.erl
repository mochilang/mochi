#!/usr/bin/env escript
% string_contains.erl - generated from string_contains.mochi

main(_) ->
    S = "catch",
    io:format("~p~n", [string:str(S, "cat") > 0]),
    io:format("~p~n", [string:str(S, "dog") > 0]).
