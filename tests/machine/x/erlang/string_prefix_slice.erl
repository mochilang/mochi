#!/usr/bin/env escript
%% Generated by Mochi compiler v0.10.27 on 2025-07-17T08:58:54Z
% string_prefix_slice.erl - generated from string_prefix_slice.mochi

main(_) ->
    io:format("~p~n", [(lists:sublist("forest", (0)+1, (length("fore"))-(0)) == "fore")]),
    io:format("~p~n", [(lists:sublist("desert", (0)+1, (length("fore"))-(0)) == "fore")]).
