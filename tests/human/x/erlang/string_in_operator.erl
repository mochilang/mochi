#!/usr/bin/env escript
%% string_in_operator.erl - manual translation of tests/vm/valid/string_in_operator.mochi

main(_) ->
    S = "catch",
    io:format("~p~n", [contains(S, "cat")]),
    io:format("~p~n", [contains(S, "dog")]).

contains(Str, Sub) ->
    case string:str(Str, Sub) of
        0 -> false;
        _ -> true
    end.
