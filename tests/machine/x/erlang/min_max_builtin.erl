#!/usr/bin/env escript
% min_max_builtin.erl - generated from min_max_builtin.mochi

main(_) ->
    Nums = [3, 1, 4],
    io:format("~p~n", [lists:min(Nums)]),
    io:format("~p~n", [lists:max(Nums)]).
