#!/usr/bin/env escript
% group_by_left_join.erl - generated from group_by_left_join.mochi

main(_) ->
    Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}],
    Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 1}, #{id => 102, customerId => 2}],
    Stats = [#{name => Key0, count => length([R || R <- Val0, maps:get(o, R)])} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{maps:get(name, C), #{c => C, o => O}} || C <- Customers, {C, O} <- mochi_left_join_item(C, Orders, fun(C, O) -> (maps:get(customerId, O) == maps:get(id, C)) end)]))],
    io:format("~p~n", ["--- Group Left Join ---"]),
    lists:foreach(fun(S) -> io:format("~p ~p ~p~n", [maps:get(name, S), "orders:", maps:get(count, S)]) end, Stats).

mochi_left_join_item(A, B, Fun) ->
    Matches = [ {A, J} || J <- B, Fun(A, J) ],
    case Matches of
        [] -> [{A, undefined}];
        _ -> Matches
    end.

mochi_left_join(L, R, Fun) ->
    lists:flatmap(fun(X) -> mochi_left_join_item(X, R, Fun) end, L).
