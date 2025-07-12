#!/usr/bin/env escript
% group_by.erl - generated from group_by.mochi

main(_) ->
    People = [#{name => "Alice", age => 30, city => "Paris"}, #{name => "Bob", age => 15, city => "Hanoi"}, #{name => "Charlie", age => 65, city => "Paris"}, #{name => "Diana", age => 45, city => "Hanoi"}, #{name => "Eve", age => 70, city => "Paris"}, #{name => "Frank", age => 22, city => "Hanoi"}],
    Stats = [#{city => Key0, count => length(Val0), avg_age => (lists:sum([maps:get(age, P) || P <- Val0]) / length([maps:get(age, P) || P <- Val0]))} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{maps:get(city, Person), Person} || Person <- People]))],
    io:format("~p~n", ["--- People grouped by city ---"]),
    lists:foreach(fun(S) -> io:format("~p ~p ~p ~p ~p~n", [maps:get(city, S), ": count =", maps:get(count, S), ", avg_age =", maps:get(avg_age, S)]) end, Stats).
