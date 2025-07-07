#!/usr/bin/env escript
%% values_builtin.erl - manual translation of tests/vm/valid/values_builtin.mochi

main(_) ->
    M = #{a => 1, b => 2, c => 3},
    io:format("~p~n", [maps:values(M)]).
