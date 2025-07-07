#!/usr/bin/env escript
%% for_map_collection.erl - manual translation of tests/vm/valid/for_map_collection.mochi

main(_) ->
    M = #{"a" => 1, "b" => 2},
    lists:foreach(fun({K,_}) -> io:format("~s~n", [K]) end, maps:to_list(M)).
