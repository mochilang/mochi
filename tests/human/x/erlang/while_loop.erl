#!/usr/bin/env escript
%% while_loop.erl - manual translation of tests/vm/valid/while_loop.mochi

main(_) ->
    loop(0).

loop(I) when I < 3 ->
    io:format("~p~n", [I]),
    loop(I + 1);
loop(_) ->
    ok.
