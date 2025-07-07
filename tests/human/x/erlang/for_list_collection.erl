#!/usr/bin/env escript
%% for_list_collection.erl - manual translation of tests/vm/valid/for_list_collection.mochi

main(_) ->
    lists:foreach(fun(N) -> io:format("~p~n", [N]) end, [1,2,3]).
