#!/usr/bin/env escript
% group_items_iteration.erl - generated from group_items_iteration.mochi

main(_) ->
    Data = [#{tag => "a", val => 1}, #{tag => "a", val => 2}, #{tag => "b", val => 3}],
    Groups = [Val0 || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{maps:get(tag, D), D} || D <- Data]))],
    Tmp0 = [],
    lists:foreach(fun(G) -> Total0 = 0, lists:foreach(fun(X) -> Total1 = (Total0 + maps:get(val, X)) end, maps:get(items, G)), Tmp1 = Tmp0 ++ [#{tag => maps:get(key, G), total => Total1}] end, Groups),
    Result = [V || {_, V} <- lists:keysort(1, [{maps:get(tag, R), R} || R <- Tmp1])],
    io:format("~p~n", [Result]).
