#!/usr/bin/env escript
%% len_map.erl - manual translation of tests/vm/valid/len_map.mochi

main(_) ->
    M = #{a => 1, b => 2},
    io:format("~p~n", [maps:size(M)]).
