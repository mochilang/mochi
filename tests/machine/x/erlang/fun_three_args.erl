#!/usr/bin/env escript
% fun_three_args.erl - generated from fun_three_args.mochi

sum3(A, B, C) ->
    ((A + B) + C).

main(_) ->
    io:format("~p~n", [sum3(1, 2, 3)]).
