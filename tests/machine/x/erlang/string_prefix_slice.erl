#!/usr/bin/env escript
% string_prefix_slice.erl - generated from string_prefix_slice.mochi

main(_) ->
    Prefix = "fore",
    S1 = "forest",
    io:format("~p~n", [(string:substr("forest", (0)+1, (length("fore"))-(0)) == "fore")]),
    S2 = "desert",
    io:format("~p~n", [(string:substr("desert", (0)+1, (length("fore"))-(0)) == "fore")]).
