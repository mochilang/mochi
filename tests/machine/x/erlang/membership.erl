#!/usr/bin/env escript
% membership.erl - generated from membership.mochi

main(_) ->
    Nums = [1, 2, 3],
    io:format("~p~n", [lists:member(2, Nums)]),
    io:format("~p~n", [lists:member(4, Nums)]).
