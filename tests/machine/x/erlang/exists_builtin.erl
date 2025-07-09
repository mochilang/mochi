#!/usr/bin/env escript
% exists_builtin.erl - generated from exists_builtin.mochi

main(_) ->
    Data = [1, 2],
    Flag = (length([X || X <- Data, (X == 1)]) > 0),
    io:format("~p~n", [Flag]).
