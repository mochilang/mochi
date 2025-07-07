#!/usr/bin/env escript
%% group_by_left_join.erl - manual translation of tests/vm/valid/group_by_left_join.mochi

main(_) ->
    Customers = [
        #{id => 1, name => "Alice"},
        #{id => 2, name => "Bob"},
        #{id => 3, name => "Charlie"}
    ],
    Orders = [
        #{id => 100, customerId => 1},
        #{id => 101, customerId => 1},
        #{id => 102, customerId => 2}
    ],
    Stats = [#{name => CName, count => count_orders(CId, Orders)} ||
                C <- Customers,
                CId = maps:get(id, C),
                CName = maps:get(name, C)],
    io:format("--- Group Left Join ---~n"),
    lists:foreach(fun(S) ->
        io:format("~s orders: ~p~n", [maps:get(name,S), maps:get(count,S)])
    end, Stats).

count_orders(Id, Orders) ->
    length([O || O <- Orders, maps:get(customerId,O) =:= Id]).
