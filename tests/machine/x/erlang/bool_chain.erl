#!/usr/bin/env escript
% bool_chain.erl - generated from bool_chain.mochi

boom() ->
    io:format("~p~n", ["boom"]),
    true.

main(_) ->
    io:format("~p~n", [((((1 < 2)) andalso ((2 < 3))) andalso ((3 < 4)))]),
    io:format("~p~n", [((((1 < 2)) andalso ((2 > 3))) andalso boom())]),
    io:format("~p~n", [(((((1 < 2)) andalso ((2 < 3))) andalso ((3 > 4))) andalso boom())]).
