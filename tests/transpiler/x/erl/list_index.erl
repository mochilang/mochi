#!/usr/bin/env escript
-module(main).
-export([main/1]).

% Generated by Mochi transpiler v0.10.33 (88ad9915a) on 2025-07-21 19:10 +0700

main(_) ->
    Xs = [10, 20, 30],
    io:format("~p~n", [lists:nth(1 + 1, Xs)]).
