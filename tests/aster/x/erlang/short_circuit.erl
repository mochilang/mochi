#!/usr/bin/env escript
-module(main).

-export([main/1]).

% Generated by Mochi transpiler v0.10.33 (88ad9915a) on 2025-07-21 19:10 +0700
boom(A, B) ->
    io:format("~s~n", ["boom"]),
    true
.

main(_) ->
    io:format("~p~n", [(false andalso boom(1, 2))]),
    io:format("~p~n", [(true orelse boom(1, 2))])
.
