#!/usr/bin/env escript
-module(main).
-export([main/1]).

% Generated by Mochi transpiler v0.10.33 (88ad9915a) on 2025-07-21 19:10 +0700

main(_) ->
    M = #{1 => "a", 2 => "b"},
    io:format("~p~n", [maps:is_key(1, M)]),
    io:format("~p~n", [maps:is_key(3, M)]).
