#!/usr/bin/env escript
%% left_join.erl - manual translation of tests/vm/valid/left_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
    Orders = [#{id => 100, customerId => 1, total => 250},
              #{id => 101, customerId => 3, total => 80}],
    Result = [begin
                C = find_customer(maps:get(customerId,O), Customers),
                #{orderId => maps:get(id,O), customer => C, total => maps:get(total,O)}
              end || O <- Orders],
    io:format("--- Left Join ---~n"),
    lists:foreach(fun(E) ->
        io:format("Order ~p customer ~p total ~p~n",
                  [maps:get(orderId,E), maps:get(customer,E), maps:get(total,E)])
    end, Result).

find_customer(Id, Cs) ->
    case lists:filter(fun(C) -> maps:get(id,C) =:= Id end, Cs) of
        [H|_] -> H;
        [] -> undefined
    end.
