#!/usr/bin/env escript
% map_int_key.erl - generated from map_int_key.mochi

main(_) ->
    M = #{1 => "a", 2 => "b"},
    io:format("~p~n", [lists:nth((1)+1, M)]).
