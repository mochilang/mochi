#!/usr/bin/env escript
%% fun_expr_in_let.erl - manual translation of tests/vm/valid/fun_expr_in_let.mochi

main(_) ->
    Square = fun(X) -> X * X end,
    io:format("~p~n", [Square(6)]).
