#!/usr/bin/env escript
% outer_join.erl - generated from outer_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}, #{id => 4, name => "Diana"}],
    Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 2, total => 125}, #{id => 102, customerId => 1, total => 300}, #{id => 103, customerId => 5, total => 80}],
    Result = [#{order => O, customer => C} || {O, C} <- mochi_outer_join(Orders, Customers, fun(O, C) -> (maps:get(customerId, O) == maps:get(id, C)) end)],
    io:format("~p~n", ["--- Outer Join using syntax ---"]),
    lists:foreach(fun(Row) -> (if maps:get(order, Row) -> (if maps:get(customer, Row) -> io:format("~p ~p ~p ~p ~p ~p~n", ["Order", maps:get(id, maps:get(order, Row)), "by", maps:get(name, maps:get(customer, Row)), "- $", maps:get(total, maps:get(order, Row))]); true -> io:format("~p ~p ~p ~p ~p ~p~n", ["Order", maps:get(id, maps:get(order, Row)), "by", "Unknown", "- $", maps:get(total, maps:get(order, Row))]) end); true -> io:format("~p ~p ~p~n", ["Customer", maps:get(name, maps:get(customer, Row)), "has no orders"]) end) end, Result).

mochi_left_join_item(A, B, Fun) ->
    Matches = [ {A, J} || J <- B, Fun(A, J) ],
    case Matches of
        [] -> [{A, undefined}];
        _ -> Matches
    end.

mochi_left_join(L, R, Fun) ->
    lists:flatmap(fun(X) -> mochi_left_join_item(X, R, Fun) end, L).

mochi_right_join_item(B, A, Fun) ->
    Matches = [ {I, B} || I <- A, Fun(I, B) ],
    case Matches of
        [] -> [{undefined, B}];
        _ -> Matches
    end.

mochi_right_join(L, R, Fun) ->
    lists:flatmap(fun(Y) -> mochi_right_join_item(Y, L, Fun) end, R).

mochi_outer_join(L, R, Fun) ->
    Left = mochi_left_join(L, R, Fun),
    Right = [ P || P = {undefined, _} <- mochi_right_join(L, R, Fun) ],
    Left ++ Right.
