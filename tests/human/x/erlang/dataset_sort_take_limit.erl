#!/usr/bin/env escript
%% dataset_sort_take_limit.erl - manual translation of tests/vm/valid/dataset_sort_take_limit.mochi

main(_) ->
    Products = [
        #{name => "Laptop", price => 1500},
        #{name => "Smartphone", price => 900},
        #{name => "Tablet", price => 600},
        #{name => "Monitor", price => 300},
        #{name => "Keyboard", price => 100},
        #{name => "Mouse", price => 50},
        #{name => "Headphones", price => 200}
    ],
    Sorted = lists:sort(fun(P1,P2) -> maps:get(price,P1) > maps:get(price,P2) end, Products),
    Rest = tl(Sorted),
    Expensive = lists:sublist(Rest, 3),
    io:format("--- Top products (excluding most expensive) ---~n"),
    lists:foreach(fun(Item) ->
        io:format("~s costs $ ~p~n", [maps:get(name,Item), maps:get(price,Item)])
    end, Expensive).
