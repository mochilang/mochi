#!/usr/bin/env escript
%% group_by_having.erl - manual translation of tests/vm/valid/group_by_having.mochi

main(_) ->
    People = [
        #{name => "Alice", city => "Paris"},
        #{name => "Bob", city => "Hanoi"},
        #{name => "Charlie", city => "Paris"},
        #{name => "Diana", city => "Hanoi"},
        #{name => "Eve", city => "Paris"},
        #{name => "Frank", city => "Hanoi"},
        #{name => "George", city => "Paris"}
    ],
    Groups = lists:foldl(fun(P,Acc) ->
                City = maps:get(city,P),
                Cnt = maps:get(City,Acc,0),
                maps:put(City,Cnt+1,Acc)
            end, #{}, People),
    Big = [#{city => City, num => Cnt} || {City,Cnt} <- maps:to_list(Groups), Cnt >= 4],
    io:format("~p~n", [Big]).
