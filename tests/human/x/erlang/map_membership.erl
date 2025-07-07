#!/usr/bin/env escript
%% map_membership.erl - manual translation of tests/vm/valid/map_membership.mochi

main(_) ->
    M = #{"a" => 1, "b" => 2},
    io:format("~p~n", [maps:is_key("a", M)]),
    io:format("~p~n", [maps:is_key("c", M)]).
