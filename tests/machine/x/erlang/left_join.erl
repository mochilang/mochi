#!/usr/bin/env escript
% left_join.erl - generated from left_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
    Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 3, total => 80}],
    Result = [#{orderId => maps:get(id, O), customer => C, total => maps:get(total, O)} || O <- Orders, {O, C} <- mochi_left_join_item(O, Customers, fun(O, C) -> (maps:get(customerId, O) == maps:get(id, C)) end)],
    io:format("~p~n", ["--- Left Join ---"]),
    lists:foreach(fun(Entry) -> io:format("~p ~p ~p ~p ~p ~p~n", ["Order", maps:get(orderId, Entry), "customer", maps:get(customer, Entry), "total", maps:get(total, Entry)]) end, Result).

mochi_left_join_item(A, B, Fun) ->
    Matches = [ {A, J} || J <- B, Fun(A, J) ],
    case Matches of
        [] -> [{A, undefined}];
        _ -> Matches
    end.

mochi_left_join(L, R, Fun) ->
    lists:flatmap(fun(X) -> mochi_left_join_item(X, R, Fun) end, L).
