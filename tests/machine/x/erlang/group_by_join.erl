#!/usr/bin/env escript
% group_by_join.erl - generated from group_by_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
    Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 1}, #{id => 102, customerId => 2}],
    Stats = [#{name => Key0, count => length(Val0)} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{maps:get(name, C), #{O=O, C=C}} || O <- Orders, C <- Customers, (maps:get(customerId, O) == maps:get(id, C))]))],
    io:format("~p~n", ["--- Orders per customer ---"]),
    lists:foreach(fun(S) -> io:format("~p ~p ~p~n", [maps:get(name, S), "orders:", maps:get(count, S)]) end, Stats).
