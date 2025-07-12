#!/usr/bin/env escript
% group_by_multi_join.erl - generated from group_by_multi_join.mochi

main(_) ->
    Nations = [#{id => 1, name => "A"}, #{id => 2, name => "B"}],
    Suppliers = [#{id => 1, nation => 1}, #{id => 2, nation => 2}],
    Partsupp = [#{part => 100, supplier => 1, cost => 10, qty => 2}, #{part => 100, supplier => 2, cost => 20, qty => 1}, #{part => 200, supplier => 1, cost => 5, qty => 3}],
    Filtered = [#{part => maps:get(part, Ps), value => (maps:get(cost, Ps) * maps:get(qty, Ps))} || Ps <- Partsupp, S <- Suppliers, N <- Nations, (maps:get(id, S) == maps:get(supplier, Ps)), (maps:get(id, N) == maps:get(nation, S)), (maps:get(name, N) == "A")],
    Grouped = [#{part => Key0, total => lists:sum([maps:get(value, R) || R <- Val0])} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{maps:get(part, X), X} || X <- Filtered]))],
    io:format("~p~n", [Grouped]).
