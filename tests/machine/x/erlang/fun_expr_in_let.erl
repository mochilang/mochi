#!/usr/bin/env escript
%% Generated by Mochi compiler v0.10.27 on 2025-07-17T08:58:19Z
% fun_expr_in_let.erl - generated from fun_expr_in_let.mochi

main(_) ->
    Square = fun(X) -> (X * X) end,
    io:format("~p~n", [Square(6)]).
