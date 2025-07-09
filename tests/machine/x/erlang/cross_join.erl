#!/usr/bin/env escript
% cross_join.erl - generated from cross_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}],
    Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 2, total => 125}, #{id => 102, customerId => 1, total => 300}],
    Result = [#{orderId => maps:get(id, O), orderCustomerId => maps:get(customerId, O), pairedCustomerName => maps:get(name, C), orderTotal => maps:get(total, O)} || O <- Orders, C <- Customers],
    io:format("~p~n", ["--- Cross Join: All order-customer pairs ---"]),
    lists:foreach(fun(Entry) -> io:format("~p ~p ~p ~p ~p ~p ~p ~p~n", ["Order", maps:get(orderId, Entry), "(customerId:", maps:get(orderCustomerId, Entry), ", total: $", maps:get(orderTotal, Entry), ") paired with", maps:get(pairedCustomerName, Entry)]) end, Result).
