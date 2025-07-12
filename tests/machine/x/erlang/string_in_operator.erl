#!/usr/bin/env escript
% string_in_operator.erl - generated from string_in_operator.mochi

main(_) ->
    S = "catch",
    io:format("~p~n", [string:str("catch", "cat") > 0]),
    io:format("~p~n", [string:str("catch", "dog") > 0]).
