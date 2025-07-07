#!/usr/bin/env escript
%% left_join_multi.erl - manual translation of tests/vm/valid/left_join_multi.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
    Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 2}],
    Items = [#{orderId => 100, sku => "a"}],
    Result = [begin
                Customer = hd([C || C <- Customers, maps:get(id,C) =:= maps:get(customerId,O)]),
                ItemOpt = [I || I <- Items, maps:get(orderId,I) =:= maps:get(id,O)],
                Item = case ItemOpt of [H|_] -> H; [] -> undefined end,
                #{orderId => maps:get(id,O), name => maps:get(name,Customer), item => Item}
              end || O <- Orders],
    io:format("--- Left Join Multi ---~n"),
    lists:foreach(fun(R) ->
        io:format("~p ~s ~p~n", [maps:get(orderId,R), maps:get(name,R), maps:get(item,R)])
    end, Result).
