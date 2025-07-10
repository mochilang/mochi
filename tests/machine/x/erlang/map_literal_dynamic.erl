#!/usr/bin/env escript
% map_literal_dynamic.erl - generated from map_literal_dynamic.mochi

main(_) ->
    X0 = 3,
    Y0 = 4,
    M0 = #{"a" => X0, "b" => Y0},
    io:format("~p ~p~n", [maps:get("a", M0), maps:get("b", M0)]).
