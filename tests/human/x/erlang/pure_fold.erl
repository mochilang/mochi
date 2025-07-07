#!/usr/bin/env escript
%% pure_fold.erl - manual translation of tests/vm/valid/pure_fold.mochi

main(_) ->
    Triple = fun(X) -> X * 3 end,
    io:format("~p~n", [Triple(1 + 2)]).
