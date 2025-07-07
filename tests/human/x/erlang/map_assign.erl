#!/usr/bin/env escript
%% map_assign.erl - manual translation of tests/vm/valid/map_assign.mochi

main(_) ->
    Scores0 = #{"alice" => 1},
    Scores1 = Scores0#{"bob" => 2},
    io:format("~p~n", [maps:get("bob", Scores1)]).
