#!/usr/bin/env escript
% pure_fold.erl - generated from pure_fold.mochi

triple(X) ->
    (X * 3).

main(_) ->
    io:format("~p~n", [triple((1 + 2))]).
