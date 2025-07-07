#!/usr/bin/env escript
%% partial_application.erl - manual translation of tests/vm/valid/partial_application.mochi

main(_) ->
    Add = fun(A, B) -> A + B end,
    Add5 = fun(B) -> Add(5, B) end,
    io:format("~p~n", [Add5(3)]).
