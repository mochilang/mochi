#!/usr/bin/env escript
% partial_application.erl - generated from partial_application.mochi

add(A, B) ->
    (A + B).

main(_) ->
    Add5 = add(5),
    io:format("~p~n", [add5(3)]).
