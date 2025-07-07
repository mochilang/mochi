#!/usr/bin/env escript
%% sort_stable.erl - manual translation of tests/vm/valid/sort_stable.mochi

main(_) ->
    Items = [
        #{n => 1, v => "a"},
        #{n => 1, v => "b"},
        #{n => 2, v => "c"}
    ],
    Sorted = lists:sort(fun(A,B) -> maps:get(n,A) =< maps:get(n,B) end, Items),
    Result = [maps:get(v,I) || I <- Sorted],
    io:format("~p~n", [Result]).
