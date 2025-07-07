#!/usr/bin/env escript
%% fun_call.erl - manual translation of tests/vm/valid/fun_call.mochi

add(A, B) ->
    A + B.

main(_) ->
    io:format("~p~n", [add(2,3)]).
