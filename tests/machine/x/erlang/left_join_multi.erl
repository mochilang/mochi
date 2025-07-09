#!/usr/bin/env escript
% left_join_multi.erl - generated from left_join_multi.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
    Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 2}],
    Items = [#{orderId => 100, sku => "a"}],
    Result = [#{orderId => maps:get(id, O), name => maps:get(name, C), item => I} || O <- Orders, C <- Customers, {O, I} <- mochi_left_join_item(O, Items, fun(O, I) -> (maps:get(id, O) == maps:get(orderId, I)) end), (maps:get(customerId, O) == maps:get(id, C))],
    io:format("~p~n", ["--- Left Join Multi ---"]),
    lists:foreach(fun(R) -> io:format("~p ~p ~p~n", [maps:get(orderId, R), maps:get(name, R), maps:get(item, R)]) end, Result).

mochi_left_join_item(A, B, Fun) ->
    Matches = [ {A, J} || J <- B, Fun(A, J) ],
    case Matches of
        [] -> [{A, undefined}];
        _ -> Matches
    end.

mochi_left_join(L, R, Fun) ->
    lists:flatmap(fun(X) -> mochi_left_join_item(X, R, Fun) end, L).
