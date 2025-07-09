#!/usr/bin/env escript
% inner_join.erl - generated from inner_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}],
    Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 2, total => 125}, #{id => 102, customerId => 1, total => 300}, #{id => 103, customerId => 4, total => 80}],
    Result = [#{orderId => maps:get(id, O), customerName => maps:get(name, C), total => maps:get(total, O)} || O <- Orders, C <- Customers, (maps:get(customerId, O) == maps:get(id, C))],
    io:format("~p~n", ["--- Orders with customer info ---"]),
    lists:foreach(fun(Entry) -> io:format("~p ~p ~p ~p ~p ~p~n", ["Order", maps:get(orderId, Entry), "by", maps:get(customerName, Entry), "- $", maps:get(total, Entry)]) end, Result).
