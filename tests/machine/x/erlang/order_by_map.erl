#!/usr/bin/env escript
% order_by_map.erl - generated from order_by_map.mochi

main(_) ->
    Data = [#{a => 1, b => 2}, #{a => 1, b => 1}, #{a => 0, b => 5}],
    Sorted = [V || {_, V} <- lists:keysort(1, [{#{a => maps:get(a, X), b => maps:get(b, X)}, X} || X <- Data])],
    io:format("~p~n", [Sorted]).
