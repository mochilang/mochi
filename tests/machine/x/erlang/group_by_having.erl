#!/usr/bin/env escript
% group_by_having.erl - generated from group_by_having.mochi

main(_) ->
    People = [#{name => "Alice", city => "Paris"}, #{name => "Bob", city => "Hanoi"}, #{name => "Charlie", city => "Paris"}, #{name => "Diana", city => "Hanoi"}, #{name => "Eve", city => "Paris"}, #{name => "Frank", city => "Hanoi"}, #{name => "George", city => "Paris"}],
    Big = [#{city => Key0, num => length(Val0)} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{maps:get(city, P), #{P=P}} || P <- People])), (length(Val0) >= 4)],
    json(Big).
