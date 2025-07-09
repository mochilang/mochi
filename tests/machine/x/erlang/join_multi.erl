#!/usr/bin/env escript
% join_multi.erl - generated from join_multi.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
    Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 2}],
    Items = [#{orderId => 100, sku => "a"}, #{orderId => 101, sku => "b"}],
    Result = [#{name => maps:get(name, C), sku => maps:get(sku, I)} || O <- Orders, C <- Customers, I <- Items, (maps:get(customerId, O) == maps:get(id, C)), (maps:get(id, O) == maps:get(orderId, I))],
    io:format("~p~n", ["--- Multi Join ---"]),
    lists:foreach(fun(R) -> io:format("~p ~p ~p~n", [maps:get(name, R), "bought item", maps:get(sku, R)]) end, Result).
