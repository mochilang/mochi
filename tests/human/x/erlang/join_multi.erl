#!/usr/bin/env escript
%% join_multi.erl - manual translation of tests/vm/valid/join_multi.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
    Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 2}],
    Items = [#{orderId => 100, sku => "a"}, #{orderId => 101, sku => "b"}],
    Result = [#{name => maps:get(name,C), sku => maps:get(sku,I)} ||
                O <- Orders,
                C <- Customers, maps:get(id,C) =:= maps:get(customerId,O),
                I <- Items, maps:get(orderId,I) =:= maps:get(id,O)],
    io:format("--- Multi Join ---~n"),
    lists:foreach(fun(R) ->
        io:format("~s bought item ~s~n", [maps:get(name,R), maps:get(sku,R)])
    end, Result).
