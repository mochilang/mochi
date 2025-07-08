#!/usr/bin/env escript
% map_in_operator.erl - generated from map_in_operator.mochi

main(_) ->
    M = #{1 => "a", 2 => "b"},
    io:format("~p~n", [maps:is_key(1, M)]),
    io:format("~p~n", [maps:is_key(3, M)]).
