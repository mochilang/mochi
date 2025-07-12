#!/usr/bin/env escript
% for_map_collection.erl - generated from for_map_collection.mochi

main(_) ->
    M0 = #{"a" => 1, "b" => 2},
    lists:foreach(fun({K,_}) -> io:format("~p~n", [K]) end, maps:to_list(M0)).
