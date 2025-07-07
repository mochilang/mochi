#!/usr/bin/env escript
%% group_by_join.erl - manual translation of tests/vm/valid/group_by_join.mochi

main(_) ->
    Customers = [
        #{id => 1, name => "Alice"},
        #{id => 2, name => "Bob"}
    ],
    Orders = [
        #{id => 100, customerId => 1},
        #{id => 101, customerId => 1},
        #{id => 102, customerId => 2}
    ],
    Joined = [{maps:get(name,C), O} || O <- Orders, C <- Customers,
                              maps:get(customerId,O) =:= maps:get(id,C)],
    Counts = lists:foldl(fun({Name,_O},Acc) ->
                Cnt = maps:get(Name,Acc,0),
                maps:put(Name,Cnt+1,Acc)
            end, #{}, Joined),
    Stats = [#{name => Name, count => Cnt} || {Name,Cnt} <- maps:to_list(Counts)],
    io:format("--- Orders per customer ---~n"),
    lists:foreach(fun(S) ->
        io:format("~s orders: ~p~n", [maps:get(name,S), maps:get(count,S)])
    end, Stats).
