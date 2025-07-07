#!/usr/bin/env escript
%% nested_function.erl - manual translation of tests/vm/valid/nested_function.mochi

main(_) ->
    Outer = fun(X) ->
        Inner = fun(Y) -> X + Y end,
        Inner(5)
    end,
    io:format("~p~n", [Outer(3)]).
