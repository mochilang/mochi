#!/usr/bin/env escript
%% for_loop.erl - manual translation of tests/vm/valid/for_loop.mochi

main(_) ->
    lists:foreach(fun(I) -> io:format("~p~n", [I]) end, lists:seq(1,3)).
