#!/usr/bin/env escript
% nested_function.erl - generated from nested_function.mochi

outer(X) ->
    Inner = fun(Y) -> (X + Y) end,
    Inner(5).

main(_) ->
    io:format("~p~n", [outer(3)]).
