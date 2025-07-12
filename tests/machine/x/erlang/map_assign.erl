#!/usr/bin/env escript
% map_assign.erl - generated from map_assign.mochi

main(_) ->
    Scores0 = #{"alice" => 1},
    Scores1 = maps:put("bob", 2, Scores0),
    io:format("~p~n", [maps:get("bob", Scores1)]).
