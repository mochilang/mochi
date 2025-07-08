#!/usr/bin/env escript
% pure_global_fold.erl - generated from pure_global_fold.mochi

inc(X) ->
    (X + K).

main(_) ->
    K = 2,
    io:format("~p~n", [inc(3)]).
