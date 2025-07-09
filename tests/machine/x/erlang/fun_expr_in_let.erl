#!/usr/bin/env escript
% fun_expr_in_let.erl - generated from fun_expr_in_let.mochi

main(_) ->
    Square = fun(X) -> (X * X) end,
    io:format("~p~n", [Square(6)]).
