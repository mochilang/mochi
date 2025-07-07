#!/usr/bin/env escript
%% map_literal_dynamic.erl - manual translation of tests/vm/valid/map_literal_dynamic.mochi

main(_) ->
    X = 3,
    Y = 4,
    M = #{"a" => X, "b" => Y},
    io:format("~p ~p~n", [maps:get("a", M), maps:get("b", M)]).
