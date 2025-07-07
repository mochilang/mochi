#!/usr/bin/env escript
%% json_builtin.erl - manual translation of tests/vm/valid/json_builtin.mochi

main(_) ->
    M = #{a => 1, b => 2},
    io:format("{\"a\":~p,\"b\":~p}\n", [maps:get(a,M), maps:get(b,M)]).
