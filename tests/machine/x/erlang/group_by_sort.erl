#!/usr/bin/env escript
% group_by_sort.erl - generated from group_by_sort.mochi

main(_) ->
    Items = [#{cat => "a", val => 3}, #{cat => "a", val => 1}, #{cat => "b", val => 5}, #{cat => "b", val => 2}],
    Grouped = [V || {_, V} <- lists:keysort(1, [{-lists:sum([maps:get(val, X) || X <- Val0]), #{cat => Key0, total => lists:sum([maps:get(val, X) || X <- Val0])}} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{maps:get(cat, I), #{I=I}} || I <- Items]))])],
    io:format("~p~n", [Grouped]).
