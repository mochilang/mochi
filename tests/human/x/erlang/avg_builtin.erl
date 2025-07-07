#!/usr/bin/env escript
%% avg_builtin.erl - manual translation of tests/vm/valid/avg_builtin.mochi

main(_) ->
    List = [1,2,3],
    Avg = lists:sum(List) / length(List),
    io:format("~p~n", [Avg]).
