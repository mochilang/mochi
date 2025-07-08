#!/usr/bin/env escript
% for_loop.erl - generated from for_loop.mochi

main(_) ->
    lists:foreach(fun(I) -> io:format("~p~n", [I]) end, lists:seq(1, (4)-1)).
