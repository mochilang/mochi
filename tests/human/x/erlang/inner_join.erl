#!/usr/bin/env escript
%% inner_join.erl - manual translation of tests/vm/valid/inner_join.mochi

main(_) ->
    Customers = [
        #{id => 1, name => "Alice"},
        #{id => 2, name => "Bob"},
        #{id => 3, name => "Charlie"}
    ],
    Orders = [
        #{id => 100, customerId => 1, total => 250},
        #{id => 101, customerId => 2, total => 125},
        #{id => 102, customerId => 1, total => 300},
        #{id => 103, customerId => 4, total => 80}
    ],
    Result = [#{orderId => Oid, customerName => Name, total => Total} ||
                O <- Orders,
                C <- Customers,
                Oid = maps:get(id,O),
                Cid = maps:get(id,C),
                Total = maps:get(total,O),
                maps:get(customerId,O) =:= Cid,
                Name = maps:get(name,C)],
    io:format("--- Orders with customer info ---~n"),
    lists:foreach(fun(E) ->
        io:format("Order ~p by ~s - $~p~n",
                  [maps:get(orderId,E), maps:get(customerName,E), maps:get(total,E)])
    end, Result).
