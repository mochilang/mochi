#!/usr/bin/env escript
% string_prefix_slice.erl - generated from string_prefix_slice.mochi

main(_) ->
    Prefix = "fore",
    S1 = "forest",
    io:format("~p~n", [(string:substr(S1, (0)+1, (length(Prefix))-(0)) == Prefix)]),
    S2 = "desert",
    io:format("~p~n", [(string:substr(S2, (0)+1, (length(Prefix))-(0)) == Prefix)]).
