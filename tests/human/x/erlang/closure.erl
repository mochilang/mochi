#!/usr/bin/env escript
%% closure.erl - manual translation of tests/vm/valid/closure.mochi

make_adder(N) ->
    fun(X) -> X + N end.

main(_) ->
    Add10 = make_adder(10),
    io:format("~p~n", [Add10(7)]).
