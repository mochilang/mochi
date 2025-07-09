#!/usr/bin/env escript
% exists_builtin.erl - generated from exists_builtin.mochi

main(_) ->
    Data = [1, 2],
    Flag = lists:any(fun(X) -> (X == 1) end, Data),
    io:format("~p~n", [Flag]).
