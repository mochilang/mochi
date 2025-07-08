#!/usr/bin/env escript
% list_index.erl - generated from list_index.mochi

main(_) ->
    Xs = [10, 20, 30],
    io:format("~p~n", [lists:nth((1)+1, Xs)]).
