#!/usr/bin/env escript
%% test_block.erl - manual translation of tests/vm/valid/test_block.mochi

main(_) ->
    X = 1 + 2,
    case X == 3 of
        true -> ok;
        false -> erlang:error(test_failed)
    end,
    io:format("ok~n").
