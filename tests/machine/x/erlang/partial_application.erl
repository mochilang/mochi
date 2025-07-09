#!/usr/bin/env escript
% partial_application.erl - generated from partial_application.mochi

add(A, B) ->
    (A + B).

main(_) ->
    Add5 = fun(P0) -> add(5, P0) end,
    io:format("~p~n", [Add5(3)]).
