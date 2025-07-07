#!/usr/bin/env escript
%% slice.erl - manual translation of tests/vm/valid/slice.mochi

main(_) ->
    io:format("~p~n", [slice_list([1,2,3], 1, 3)]),
    io:format("~p~n", [slice_list([1,2,3], 0, 2)]),
    io:format("~s~n", [slice_string("hello", 1, 4)]).

slice_list(List, Start, End) ->
    lists:sublist(lists:nthtail(Start, List), End - Start).

slice_string(Str, Start, End) ->
    string:substr(Str, Start + 1, End - Start).
