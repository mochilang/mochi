#!/usr/bin/env escript
% map_index.erl - generated from map_index.mochi

main(_) ->
    M = #{a => 1, b => 2},
    io:format("~p~n", [maps:get("b", M)]).
