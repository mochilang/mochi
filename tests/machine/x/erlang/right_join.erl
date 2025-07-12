#!/usr/bin/env escript
% right_join.erl - generated from right_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}, #{id => 4, name => "Diana"}],
    Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 2, total => 125}, #{id => 102, customerId => 1, total => 300}],
    Result = [#{customerName => maps:get(name, C), order => O} || {C, O} <- mochi_right_join(Customers, Orders, fun(C, O) -> (maps:get(customerId, O) == maps:get(id, C)) end)],
    io:format("~p~n", ["--- Right Join using syntax ---"]),
    lists:foreach(fun(Entry) -> (case maps:get(order, Entry) of true -> io:format("~p ~p ~p ~p ~p ~p~n", ["Customer", maps:get(customerName, Entry), "has order", maps:get(id, maps:get(order, Entry)), "- $", maps:get(total, maps:get(order, Entry))]); _ -> io:format("~p ~p ~p~n", ["Customer", maps:get(customerName, Entry), "has no orders"]) end) end, Result).

mochi_right_join_item(B, A, Fun) ->
    Matches = [ {I, B} || I <- A, Fun(I, B) ],
    case Matches of
        [] -> [{undefined, B}];
        _ -> Matches
    end.

mochi_right_join(L, R, Fun) ->
    lists:flatmap(fun(Y) -> mochi_right_join_item(Y, L, Fun) end, R).
