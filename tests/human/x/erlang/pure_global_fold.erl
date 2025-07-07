#!/usr/bin/env escript
%% pure_global_fold.erl - manual translation of tests/vm/valid/pure_global_fold.mochi

main(_) ->
    K = 2,
    Inc = fun(X) -> X + K end,
    io:format("~p~n", [Inc(3)]).
