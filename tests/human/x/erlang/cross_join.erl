#!/usr/bin/env escript
%% cross_join.erl - manual translation of tests/vm/valid/cross_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"},
                 #{id => 2, name => "Bob"},
                 #{id => 3, name => "Charlie"}],
    Orders = [#{id => 100, customerId => 1, total => 250},
              #{id => 101, customerId => 2, total => 125},
              #{id => 102, customerId => 1, total => 300}],
    Result = [#{orderId => O#{id}, orderCustomerId => O#{customerId},
                pairedCustomerName => C#{name}, orderTotal => O#{total}} ||
              O <- Orders, C <- Customers],
    io:format("--- Cross Join: All order-customer pairs ---~n"),
    lists:foreach(fun(E) ->
        io:format("Order ~p (customerId: ~p, total: $ ~p) paired with ~s~n",
                  [maps:get(orderId, E), maps:get(orderCustomerId, E),
                   maps:get(orderTotal, E), maps:get(pairedCustomerName, E)])
    end, Result).
