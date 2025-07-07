#!/usr/bin/env escript
%% string_prefix_slice.erl - manual translation of tests/vm/valid/string_prefix_slice.mochi

main(_) ->
    Prefix = "fore",
    S1 = "forest",
    io:format("~p~n", [starts_with(S1, Prefix)]),
    S2 = "desert",
    io:format("~p~n", [starts_with(S2, Prefix)]).

starts_with(Str, Prefix) ->
    string:substr(Str, 1, length(Prefix)) =:= Prefix.
