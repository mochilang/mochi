#!/usr/bin/env escript
% closure.erl - generated from closure.mochi

makeAdder(N) ->
    fun(X) -> (X + N) end.

main(_) ->
    Add10 = makeAdder(10),
    io:format("~p~n", [Add10(7)]).
