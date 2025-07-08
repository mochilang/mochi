#!/usr/bin/env escript
% avg_builtin.erl - generated from avg_builtin.mochi

main(_) ->
    io:format("~p~n", [(lists:sum([1, 2, 3]) / length([1, 2, 3]))]).
