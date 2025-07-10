#!/usr/bin/env escript
% test_block.erl - generated from test_block.mochi

main(_) ->
    X = (1 + 2), (case (X == 3) of true -> ok; _ -> erlang:error(test_failed) end),
    io:format("~p~n", ["ok"]).
