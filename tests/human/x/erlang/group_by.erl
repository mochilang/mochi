#!/usr/bin/env escript
%% group_by.erl - manual translation of tests/vm/valid/group_by.mochi

main(_) ->
    People = [
        #{name => "Alice", age => 30, city => "Paris"},
        #{name => "Bob", age => 15, city => "Hanoi"},
        #{name => "Charlie", age => 65, city => "Paris"},
        #{name => "Diana", age => 45, city => "Hanoi"},
        #{name => "Eve", age => 70, city => "Paris"},
        #{name => "Frank", age => 22, city => "Hanoi"}
    ],
    Acc0 = #{},
    Groups = lists:foldl(fun(P,Acc) ->
                City = maps:get(city,P),
                Age = maps:get(age,P),
                {Cnt0,Sum0} = maps:get(City,Acc,{0,0}),
                maps:put(City,{Cnt0+1,Sum0+Age},Acc)
            end, Acc0, People),
    Stats = [#{city => C, count => Cnt, avg_age => Sum/Cnt} || {C,{Cnt,Sum}} <- maps:to_list(Groups)],
    io:format("--- People grouped by city ---~n"),
    lists:foreach(fun(S) ->
        io:format("~s: count = ~p, avg_age = ~p~n",
                  [maps:get(city,S), maps:get(count,S), maps:get(avg_age,S)])
    end, Stats).
