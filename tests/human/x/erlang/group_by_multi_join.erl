#!/usr/bin/env escript
%% group_by_multi_join.erl - manual translation of tests/vm/valid/group_by_multi_join.mochi

main(_) ->
    Nations = [
        #{id => 1, name => "A"},
        #{id => 2, name => "B"}
    ],
    Suppliers = [
        #{id => 1, nation => 1},
        #{id => 2, nation => 2}
    ],
    Partsupp = [
        #{part => 100, supplier => 1, cost => 10.0, qty => 2},
        #{part => 100, supplier => 2, cost => 20.0, qty => 1},
        #{part => 200, supplier => 1, cost => 5.0, qty => 3}
    ],
    Filtered = [#{part => PSPart, value => PSVal} ||
        PS <- Partsupp,
        S <- Suppliers, maps:get(id,S) =:= maps:get(supplier,PS),
        N <- Nations, maps:get(id,N) =:= maps:get(nation,S),
        maps:get(name,N) =:= "A",
        PSPart = maps:get(part,PS),
        PSVal = maps:get(cost,PS) * maps:get(qty,PS)],
    Groups = lists:foldl(fun(Item,Acc) ->
                Part = maps:get(part,Item),
                Val = maps:get(value,Item),
                Sum = maps:get(Part,Acc,0),
                maps:put(Part, Sum + Val, Acc)
            end, #{}, Filtered),
    Result = [#{part => P, total => T} || {P,T} <- maps:to_list(Groups)],
    io:format("~p~n", [Result]).
