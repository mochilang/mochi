#!/usr/bin/env escript
%% map_int_key.erl - manual translation of tests/vm/valid/map_int_key.mochi

main(_) ->
    M = #{1 => "a", 2 => "b"},
    io:format("~s~n", [maps:get(1, M)]).
